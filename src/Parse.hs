{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Parse where

import Prelude hiding (many)
import Control.Monad.Combinators.NonEmpty as NE
import Data.Char
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Builder
import Text.Megaparsec hiding (parse, runParser)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (scientific)

import NonEmptyText
import Syntax
import Value

data Delimiters = Delimiters
  { begin :: NonEmptyText
  , end :: NonEmptyText }

data PartialStatement
  = BlockS ([Statement] -> Statement)
  | StandaloneS Statement

newtype Parser a = Parser
  { unParser :: ParsecT Void Text (Reader Delimiters) a }
  deriving ( Monad, Applicative, Functor, Alternative, MonadPlus )

deriving instance MonadParsec Void Text Parser

defaultDelimiters :: Delimiters
defaultDelimiters = Delimiters "{" "}"

-- TODO: put this somewhere else.
parse :: Text -> Either String [Statement]
parse =
  first errorBundlePretty .
  runParser defaultDelimiters templateP "goof"

runParser :: Delimiters -> Parser a -> FilePath -> Text -> Either (ParseErrorBundle Text Void) a
runParser delims (Parser parser) name input =
  let r = runParserT parser name input
  in runReader r delims

getBeginDelim :: Parser Text
getBeginDelim =
  Parser (asks (fromNonEmptyText . begin))

getEndDelim :: Parser Text
getEndDelim =
  Parser (asks (fromNonEmptyText . end))

blockP, templateP :: Parser [Statement]
blockP = syntaxP True
templateP = syntaxP False

syntaxP :: Bool -> Parser [Statement]
syntaxP inBlock = do
  return [] <* endP
    <|> ((:) <$> statementP <*> syntaxP inBlock)
    <|> ((:) <$> fmap VerbatimS ((<>) <$> (fmap Builder.singleton anySingle) <*> verbatimP) <*> syntaxP inBlock)
  where
    verbatimP = do
      beginD <- getBeginDelim
      Builder.fromText <$> takeWhileP (Just "any non-delimiter character") (/= Text.head beginD)
    endP =
      if inBlock then try (withinDelims (keywordP "end") <?> "end of block") else eof

statementP :: Parser Statement
statementP = label "statement" $ do
  statem <- withinDelims $
    forP <|> (StandaloneS . ExprS) <$> exprP <|> emptyP
  case statem of
    BlockS continuation ->
      continuation <$> blockP
    StandaloneS stat ->
      return stat

withinDelims :: Parser a -> Parser a
withinDelims innerP = do
  chunk =<< getBeginDelim
  space
  inner <- innerP
  chunk =<< getEndDelim
  return inner

isIdChar :: Char -> Bool
isIdChar = \c -> isAlpha c || c == '-' || c == '_'

keywordP :: Text -> Parser ()
keywordP ident = do
  chunk ident
  notFollowedBy (satisfy isIdChar)
  space

nameP :: Parser Name
nameP = label "name" $
  -- TODO: fail if is keyword.
  takeWhile1P (Just "identifier character") isIdChar <* space

forP :: Parser PartialStatement
forP = do
  keywordP "for"
  itemName <- nameP
  keywordP "in"
  array <- exprP
  return (BlockS $ ForS itemName array)

emptyP :: Parser PartialStatement
emptyP =
  return (StandaloneS EmptyS) <?> "an empty statement"

exprP :: Parser Expr
exprP = do
  (a1 :| atoms) <- NE.some atomicExprP
  return (foldl' (\e a -> Expr (ApplyE e a)) a1 atoms)

atomicExprP :: Parser Expr
atomicExprP = do
  expr <-
    parensP exprP
    <|> Expr . ArrayE <$> bracketsP (sepEndBy exprP (specialCharP ','))
    <|> Expr . LiteralE <$> numberP
    <|> Expr . NameE <$> nameP
  fields <- many (specialCharP '.' *> nameP)
  return (foldl' (\e f -> Expr (FieldAccessE f e)) expr fields)
 where
  parensP = between (specialCharP '(') (specialCharP ')')
  bracketsP = between (specialCharP '[') (specialCharP ']')

numberP :: Parser Literal
numberP = NumberL <$> scientific <* space

-- TODO: parse other kinds of exprs!

specialCharP :: Char -> Parser ()
specialCharP c = single c >> space
