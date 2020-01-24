{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Parse where

import Prelude hiding (many)
import Data.Char
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Builder
import Text.Megaparsec
import Text.Megaparsec.Char

import NonEmptyText
import Syntax
import Value

data Delimiters = Delimiters
  { begin :: NonEmptyText
  , end :: NonEmptyText }

newtype Parser a = Parser
  { unParser :: ParsecT Void Text (Reader Delimiters) a }
  deriving ( Monad, Applicative, Functor, Alternative, MonadPlus )

deriving instance MonadParsec Void Text Parser

getBeginDelim :: Parser Text
getBeginDelim =
  Parser (asks (fromNonEmptyText . begin))

getEndDelim :: Parser Text
getEndDelim =
  Parser (asks (fromNonEmptyText . end))

blockP, templateP :: Parser [Syntax]
blockP = syntaxP True
templateP = syntaxP False

syntaxP :: Bool -> Parser [Syntax]
syntaxP inBlock =
  let endP =
        return EmptyS <*
        if inBlock then withinDelims (keywordP "end") else eof
  in many (VerbatimS  <$> verbatimP
       <|> StatementS <$> endP
       <|> StatementS <$> statementP)

verbatimP :: Parser Verbatim
verbatimP = do
  beginD <- getBeginDelim
  verbatim1 <-
    Builder.fromText <$> takeWhileP Nothing (/= Text.head beginD)
  (lookAhead (chunk beginD) >> return verbatim1)
    <|> fmap (verbatim1 <>) ((<>) <$> (fmap Builder.singleton anySingle) <*> verbatimP)

statementP :: Parser Statement
statementP = do
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
nameP =
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
exprP = NameE <$> nameP
-- TODO: parse other kinds of exprs!
