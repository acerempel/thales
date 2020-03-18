{-|
Description : The parser for this template language.

This is the parser! You use it by calling the 'parseTemplate' function with the
appropriate arguments. You must select the delimiters (which set template
statments and expressions off from text to be included verbatim) yourself; this
is the 'Delimiters' datatype.

A small handful of other names are exported – these are just so that they can be
tested; consumers of this module should not normally need them.
-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Parse
  ( Parser, runParser, parse, parseTemplate
  , templateP, exprP
  , Delimiters(..), defaultDelimiters
  , CustomError(..)
  )
where

import Prelude hiding (many)
import Data.Char
import qualified Data.Text as Text
import Text.Megaparsec hiding (parse, runParser)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (scientific)

import qualified List
import NonEmptyText (NonEmptyText(..))
import qualified NonEmptyText
import Syntax

{-| The strings that delimit bits of code, or directives, or whatever
you want to call them, in a template. E.g. @Delimiters "{{" "}}"@,
or @Delimiters { begin = "$(", end = ")" }@. -}
data Delimiters = Delimiters
  { begin :: NonEmptyText
  , end :: NonEmptyText }
  deriving Show

-- | A statement that may enclose a block of further statements (or may not).
data PartialStatement
  -- | A block statement -- takes a block of statements as an argument, like
  -- @for@. An @end@ statement signifies the end of the block.
  = BlockS ([Statement] -> Statement)
  -- | A standalone statement, like an expression.
  | StandaloneS Statement

{-| Our parser monad. This is a newtype over Megaparsec's 'ParsecT' type. -}
newtype Parser a = Parser
  { unParser :: ParsecT CustomError Text (Reader Delimiters) a }
  deriving newtype ( Monad, Applicative, Functor, Alternative, MonadPlus )

deriving newtype instance MonadParsec CustomError Text Parser

data CustomError
  -- | This error constructor signifies that an internal invariant in the parser was
  -- broken. If you ever see it in an error message, it's a bug in the parser.
  = InternalError
  | UnknownFunction Text
  deriving stock ( Ord, Eq, Show )

instance ShowErrorComponent CustomError where
  showErrorComponent InternalError = "Internal error!"
  showErrorComponent (UnknownFunction name) =
    Text.unpack name <> " is not a known function."

{-| A default set of delimiters – @{{ ... }}@, same as what Mustache templates
use (and Jinja2 and Liquid, for expression splices). -}
defaultDelimiters :: Delimiters
defaultDelimiters = Delimiters "{{" "}}"

-- TODO: put this somewhere else.
{-| For ad-hoc manual testing purposes. -}
parse :: Text -> Either String [Statement]
parse =
  first errorBundlePretty .
  runParser templateP defaultDelimiters "goof"

{-| Parse a template. This is simple @'runParser' 'templateP'@. -}
parseTemplate ::
  Delimiters ->
  -- | The filename -- used for error messages.
  FilePath ->
  -- | The input.
  Text ->
  Either (ParseErrorBundle Text CustomError) [Statement]
parseTemplate =
  runParser templateP

{-| Run an arbitrary parser. Currently this module only exports two parsers,
namely 'templateP' and 'exprP', for parsing an entire template and an
expression respectively.-}
runParser ::
  -- | The parser you wish to run.
  Parser a ->
  Delimiters ->
  -- | The filename -- used for error messages.
  FilePath ->
  -- | The input to the parser.
  Text ->
  Either (ParseErrorBundle Text CustomError) a
runParser parser delims name input =
  let r = runParserT (unParser parser) name input
  in runReader r delims

getBeginDelim :: Parser NonEmptyText
getBeginDelim =
  Parser (asks begin)

getEndDelim :: Parser NonEmptyText
getEndDelim =
  Parser (asks end)

blockP, templateP :: Parser [Statement]
blockP = syntaxP True
-- | Parser for an entire template. Used in 'parseTemplate'.
templateP = syntaxP False

syntaxP :: Bool -> Parser [Statement]
syntaxP inBlock =
  [] <$ endP
    <|> ((:) <$> statementP <*> syntaxP inBlock)
    <|> ((:) <$> fmap VerbatimS
                 (NonEmptyText <$> anySingle <*> verbatimP)
             <*> syntaxP inBlock)
  where
    verbatimP = do
      beginD <- getBeginDelim
      takeWhileP (Just "any non-delimiter character") (/= NonEmptyText.head beginD)
    endP =
      -- TODO: I do not like this 'try'. Would be nice to remove it.
      if inBlock then try (withinDelims (keywordP "end") <?> "end of block") else eof

statementP :: Parser Statement
statementP = label "statement" $ do
  statem <- withinDelims $ do
    sp <- getSourcePos
    forP sp <|> optionallyP sp <|> StandaloneS . ExprS sp <$> exprP
  case statem of
    BlockS continuation ->
      continuation <$> blockP
    StandaloneS stat ->
      return stat

withinDelims :: Parser a -> Parser a
withinDelims innerP = do
  chunk . NonEmptyText.toText =<< getBeginDelim
  space
  inner <- innerP
  chunk . NonEmptyText.toText =<< getEndDelim
  return inner

isIdChar :: Char -> Bool
isIdChar = \c -> isAlpha c || c == '-' || c == '_'

keywordP :: Text -> Parser ()
keywordP ident = do
  chunk ident
  notFollowedBy (satisfy isIdChar)
  space

nameP :: Parser Name
nameP = label "name" $ do
  -- TODO: fail if is keyword.
  ident <- takeWhile1P (Just "identifier character") isIdChar <* space
  return $ Name ident

forP :: SourcePos -> Parser PartialStatement
forP sp = do
  keywordP "for"
  itemName <- nameP
  keywordP "in"
  array <- exprP
  return (BlockS $ ForS sp itemName array)

optionallyP :: SourcePos -> Parser PartialStatement
optionallyP sp = do
  keywordP "optionally"
  expr <- exprP
  mb_name <- optional $ do
    keywordP "as"
    nameP
  return (BlockS $ OptionallyS sp expr mb_name)

-- TODO: Parsing of record literals, record updates, array indexes, add back
-- some form of application.
{-| Parse an expression. Only exported for testing purposes. -}
exprP :: Parser Expr
exprP = do
  expr <-
    parensP exprP
    <|> ArrayE . coerce . List.fromList
        <$> bracketsP (sepEndBy exprP (specialCharP ','))
    <|> LiteralE <$> numberP
    <|> LiteralE <$> stringP
    <|> applicationOrNameP
  fields <- many (specialCharP '.' *> nameP)
  return (foldl' (\e f -> FieldAccessE f (Id e)) expr fields)
 where
  parensP = between (specialCharP '(') (specialCharP ')')
  bracketsP = between (specialCharP '[') (specialCharP ']')
  applicationOrNameP = do
    name <- nameP
    mb_arg <- optional exprP
    case mb_arg of
      Nothing ->
        pure (NameE name)
      Just arg
        | Just funcConstr <- identifyFunction name ->
          pure (funcConstr arg)
        | otherwise ->
          customFailure (UnknownFunction (fromName name))

numberP :: Parser Literal
numberP = NumberL <$> scientific <* space

stringP :: Parser Literal
stringP = do
  specialCharP '"'
  str <- takeWhileP (Just "any character other than '\"'") (/= '"')
  specialCharP '"'
  return (StringL str)

specialCharP :: Char -> Parser ()
specialCharP c = single c >> space
