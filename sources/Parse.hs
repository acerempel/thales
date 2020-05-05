{-|
Description : The parser for this template language.

This is the parser! You use it by calling the 'parseTemplate' function with the
appropriate arguments. You must select the delimiters (which set template
statments and expressions off from text to be included verbatim) yourself; this
is the 'Delimiters' datatype.

A small handful of other names are exported – these are just so that they can be
tested; consumers of this module should not normally need them.
-}
{-# OPTIONS_GHC -Wno-unused-do-bind -Wno-missing-signatures -Wmissing-exported-signatures #-}
module Parse
  ( Parser, runParser, parse, parseTemplate
  , templateP, exprP
  , CustomError(..)
  , ParsedTemplate(..)
  )
where

import Prelude hiding (many)
import Data.Char
import qualified Data.HashSet as Set
import qualified Data.Text as Text
import Text.Megaparsec hiding (parse, runParser)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (scientific)
-- import Text.Megaparsec.Debug

import qualified List
import Syntax

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

-- TODO: put this somewhere else.
{-| For ad-hoc manual testing purposes. -}
parse :: Text -> Either String [Statement]
parse =
  first errorBundlePretty .
  runParser templateP defaultDelimiters "goof"

data ParsedTemplate = ParsedTemplate
  { templateDelimiters :: Delimiters
  , templatePath :: FilePath
  , templateStatements :: [Statement] }
  deriving ( Show )

{-| Parse a template. This is simple @'runParser' 'templateP'@. -}
parseTemplate ::
  Delimiters ->
  -- | The filename -- used for error messages.
  FilePath ->
  -- | The input.
  Text ->
  Either (ParseErrorBundle Text CustomError) ParsedTemplate
parseTemplate delimiters path input =
  ParsedTemplate delimiters path <$> runParser templateP delimiters path input

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

getBeginDelim :: Parser Text
getBeginDelim =
  Parser (asks begin)

getEndDelim :: Parser Text
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
    <|> ((:) <$> (VerbatimS <$> anySingle <*> verbatimP)
             <*> syntaxP inBlock)
  where
    verbatimP = do
      beginD <- getBeginDelim
      takeWhileP (Just "any non-delimiter character") (/= Text.head beginD)
    endP =
      -- TODO: I do not like this 'try'. Would be nice to remove it.
      if inBlock then try (withinDelims (keywordP "end") <?> "end of block") else eof

statementP :: Parser Statement
statementP = label "statement" $ do
  statem <- withinDelims $ do
    sp <- getSourcePos
    forP sp
      <|> optionallyP sp
      <|> letP sp
      <|> exportP sp
      <|> includeBodyP sp
      <|> StandaloneS . ExprS sp <$> exprP
  case statem of
    BlockS continuation ->
      continuation <$> blockP
    StandaloneS stat ->
      return stat

withinDelims :: Parser a -> Parser a
withinDelims innerP = do
  label "opening delimiter" $ chunk =<< getBeginDelim
  space
  inner <- innerP
  label "closing delimiter" $ chunk =<< getEndDelim
  return inner

isIdChar :: Char -> Bool
isIdChar = \c -> isAlpha c || c == '-' || c == '_'

keywordP :: Text -> Parser ()
keywordP ident = do
  chunk ident
  notFollowedBy (satisfy isIdChar)
  space

-- | Parse an arbitrary identifier.
anyNameP :: Parser Name
anyNameP = do
  ident <- takeWhile1P (Just "identifier character") isIdChar <* space
  return $ Name ident

-- | Parse a user-defined identifier, i.e., cannot be a keyword.
nameP :: Parser Name
nameP = label "name" $ do
  Name name <- anyNameP
  if name `Set.member` keywords
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

-- | Parse an 'export' statement with some bindings – puns work too.
exportP :: SourcePos -> Parser PartialStatement
exportP sp = do
  keywordP "provide"
  StandaloneS . ExportS sp <$> sepEndBy recordBindingP comma

includeBodyP :: SourcePos -> Parser PartialStatement
includeBodyP sp = do
  keywordP "include-body"
  StandaloneS . IncludeBodyS sp <$> exprP

-- TODO: Parsing of record updates, array indexes
{-| Parse an expression. Only exported for testing purposes. -}
exprP :: Parser Expr
exprP = label "an expression" $ do
  expr <-
    parensP exprP
    <|> ArrayE . coerce . List.fromList
        <$> bracketsP (sepEndBy exprP comma)
    <|> RecordE <$> bracesP (sepEndBy recordBindingP comma)
    <|> LiteralE <$> numberP
    <|> LiteralE <$> stringP
    <|> functionCallP
    <|> LiteralE EmptyL <$ keywordP "empty"
    <|> NameE <$> nameP
  fields <- many (dot *> liftA2 (,) nameP (optional (specialCharP '?')))
  return (foldl' (\e (fieldName, isOptional) ->
    FieldAccessE fieldName (IsOptional (isJust isOptional)) (Rec e)) expr fields)

parensP = betweenChars '(' ')'
bracketsP = betweenChars '[' ']'
bracesP = betweenChars '{' '}'

functionCallP :: Parser Expr
functionCallP = do
  name <- try $ nameP <* lookAhead (char '(')
  args <- parensP (sepEndBy exprP comma)
  pure (FunctionCallE name (coerce args))

recordBindingP :: Parser (RecordBinding (Rec ExprF))
recordBindingP = do
  name <- nameP
  mb_val <- optional (equals >> exprP)
  pure $
    case mb_val of
      Just val -> FieldAssignment name (Rec val)
      Nothing  -> FieldPun name

keywords :: HashSet Text
keywords = Set.fromList ["let", "in", "provide", "optionally", "for", "include-body", "empty"]

numberP :: Parser Literal
numberP = NumberL <$> scientific <* space

stringP :: Parser Literal
stringP = do
  specialCharP '"'
  StringL <$> stringContents
 where
  stringContents = do
    str <- takeWhileP (Just "string contents") (\c -> c /= '"' && c /= '\\')
    (specialCharP '"' >> return str)
      <|> (single '\\' >> anySingle >>= \c -> ((str <> escaped c) <>) <$> stringContents)
  escaped c =
    case c of
      '"' -> "\""
      '\\' -> "\\"
      _ -> Text.pack ['\\', c]

betweenChars :: Char -> Char -> Parser a -> Parser a
betweenChars before after =
  between (specialCharP before) (specialCharP after)

comma, equals, dot :: Parser ()
comma = specialCharP ','
equals = specialCharP '='
dot = specialCharP '.'

specialCharP :: Char -> Parser ()
specialCharP c = single c >> space

-- debug :: Show a => String -> Parser a -> Parser a
-- debug lbl = Parser . dbg lbl . unParser
