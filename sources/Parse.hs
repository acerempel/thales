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
import Data.Binary
import Data.Char
import qualified Data.HashSet as Set
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
  deriving stock ( Show, Eq, Generic, Typeable )
  deriving anyclass ( Hashable, NFData, Binary )

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
  = UnknownFunction Text
  | IsFunctionName Text
  | WrongNumberOfArguments Text
  deriving stock ( Ord, Eq, Show )

instance ShowErrorComponent CustomError where
  showErrorComponent (UnknownFunction name) =
    Text.unpack name <> " is not a known function."
  showErrorComponent (IsFunctionName name) =
    Text.unpack name <> " is the name of a function!"
  showErrorComponent (WrongNumberOfArguments name) =
    Text.unpack name <> " was given the wrong number of arguments."

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
    forP sp <|> optionallyP sp <|> letP sp
      <|> exportP sp
      <|> StandaloneS . ExprS sp <$> exprP
  case statem of
    BlockS continuation ->
      continuation <$> blockP
    StandaloneS stat ->
      return stat

withinDelims :: Parser a -> Parser a
withinDelims innerP = do
  label "opening delimiter" $ nonEmptyChunk =<< getBeginDelim
  space
  inner <- innerP
  label "closing delimiter" $ nonEmptyChunk =<< getEndDelim
  return inner
 where
  nonEmptyChunk (NonEmptyText hd tl) =
    try $ char hd >> chunk tl

isIdChar :: Char -> Bool
isIdChar = \c -> isAlpha c || c == '-' || c == '_'

keywordP :: Text -> Parser ()
keywordP ident = do
  chunk ident
  notFollowedBy (satisfy isIdChar)
  space

anyNameP :: Parser Name
anyNameP = do
  ident <- takeWhile1P (Just "identifier character") isIdChar <* space
  return $ Name ident

nameP :: Parser Name
nameP = label "name" $ do
  Name name <- anyNameP
  if isJust (identifyFunction name) || name `Set.member` keywords
    then customFailure (IsFunctionName name) -- TODO better error message
    else return (Name name)

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

-- | Parse the header of a 'let' block, of the form @let name = expr, … in@.
-- Note that a trailing comma after the bindings is optional, and that it is
-- permissible to provide no bindings (@let in@).
letP :: SourcePos -> Parser PartialStatement
letP sp = do
  keywordP "let"
  bindings <- sepEndBy binding comma
  keywordP "in"
  return (BlockS $ LetS sp bindings)
 where
  binding =
    (,) <$> (nameP <* equals) <*> exprP

exportP :: SourcePos -> Parser PartialStatement
exportP sp = do
  keywordP "export"
  StandaloneS . ExportS sp <$> sepEndBy recordBindingP comma

-- TODO: Parsing of record updates, array indexes
{-| Parse an expression. Only exported for testing purposes. -}
exprP :: Parser Expr
exprP =
  -- TODO reorganize this, maybe
  fieldAccessP <|> applicationOrNameP <|> atomicExprP
 where
  fieldAccessP = do
    name <- try nameP
    fields <- many (dot *> nameP)
    return (foldl' (\e f -> FieldAccessE f (Id e)) (NameE name) fields)
  -- TODO: Rewrite to improve error messages.
  applicationOrNameP = do
    Name name <- anyNameP
    arguments <- many atomicExprP
    case (identifyFunction name, arguments) of
      (Nothing, []) ->
        pure $ NameE (Name name)
      (Just (OneArgumentFunction f), [arg1]) ->
        pure $ f arg1
      (Just (TwoArgumentFunction f), [arg1, arg2]) ->
        pure $ f arg1 arg2
      (Just _, (_ : _)) ->
        customFailure (WrongNumberOfArguments name)
      (Just _, []) ->
        customFailure (IsFunctionName name)
      (Nothing, (_ : _)) ->
        customFailure (UnknownFunction name)

atomicExprP :: Parser Expr
atomicExprP = label "an expression" $
    parensP exprP
    <|> ArrayE . coerce . List.fromList
        <$> bracketsP (sepEndBy exprP comma)
    <|> RecordE <$> bracesP (sepEndBy recordBindingP comma)
    <|> LiteralE <$> numberP
    <|> LiteralE <$> stringP
 where
  parensP = betweenChars '(' ')'
  bracketsP = betweenChars '[' ']'
  bracesP = betweenChars '{' '}'

recordBindingP :: Parser (RecordBinding Id)
recordBindingP = do
  name <- nameP
  mb_val <- optional (equals >> exprP)
  pure $
    case mb_val of
      Just val -> FieldAssignment name (Id val)
      Nothing  -> FieldPun name

data Function
  = OneArgumentFunction (Expr -> Expr)
  | TwoArgumentFunction (Expr -> Expr -> Expr)

identifyFunction :: Text -> Maybe Function
identifyFunction name =
  case name of
    "load-yaml" ->
      Just $ OneArgumentFunction (FileE YamlFile . Id)
    "load-markdown" ->
      Just $ OneArgumentFunction (FileE MarkdownFile . Id)
    "list-directory" ->
      Just $ OneArgumentFunction (ListDirectoryE . Id)
    "load-template" ->
      Just $ TwoArgumentFunction (\path binds -> FileE (TemplateFile (Id binds)) (Id path))
    _ ->
      Nothing

keywords :: HashSet Text
keywords = Set.fromList ["let", "in", "export", "optionally", "for"]

numberP :: Parser Literal
numberP = NumberL <$> scientific <* space

stringP :: Parser Literal
stringP = do
  specialCharP '"'
  str <- takeWhileP (Just "any character other than '\"'") (/= '"')
  specialCharP '"'
  return (StringL str)

betweenChars :: Char -> Char -> Parser a -> Parser a
betweenChars before after =
  between (specialCharP before) (specialCharP after)

comma, equals, dot :: Parser ()
comma = specialCharP ','
equals = specialCharP '='
dot = specialCharP '.'

specialCharP :: Char -> Parser ()
specialCharP c = single c >> space
