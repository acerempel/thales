{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Parse
  ( Parser, runParser, parse, parseTemplate
  , templateP, exprP
  , Delimiters(..), defaultDelimiters
  )
where

import Prelude hiding (many)
import Control.Monad.Combinators.NonEmpty as NE
import Data.Char
import qualified Data.Text as Text
import qualified Data.Vector as Vec
import Text.Megaparsec hiding (parse, runParser)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (scientific)

import NonEmptyText
import Syntax
import Value
import Verbatim

data Delimiters = Delimiters
  { begin :: NonEmptyText
  , end :: NonEmptyText }
  deriving Show

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
  runParser templateP defaultDelimiters "goof"

parseTemplate :: Delimiters -> FilePath -> Text -> Either (ParseErrorBundle Text Void) [Statement]
parseTemplate =
  runParser templateP

runParser :: Parser a -> Delimiters -> FilePath -> Text -> Either (ParseErrorBundle Text Void) a
runParser parser delims name input =
  let r = runParserT (unParser parser) name input
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
    <|> ((:) <$> fmap VerbatimS ((<>) <$> (fmap preEscapedSingleton anySingle) <*> verbatimP) <*> syntaxP inBlock)
  where
    verbatimP = do
      beginD <- getBeginDelim
      preEscaped <$> takeWhileP (Just "any non-delimiter character") (/= Text.head beginD)
    endP =
      -- TODO: I do not like this 'try'. Would be nice to remove it.
      if inBlock then try (withinDelims (keywordP "end") <?> "end of block") else eof

statementP :: Parser Statement
statementP = label "statement" $ do
  statem <- withinDelims $ do
    sp <- getSourcePos
    forP sp <|> (StandaloneS . ExprS sp) <$> exprP
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

forP :: SourcePos -> Parser PartialStatement
forP sp = do
  keywordP "for"
  itemName <- nameP
  keywordP "in"
  array <- exprP
  return (BlockS $ ForS sp itemName array)

exprP :: Parser Expr
exprP = do
  (a1 :| atoms) <- NE.some atomicExprP
  return (foldl' (\e a -> ApplyE (Id e) (Id a)) a1 atoms)

atomicExprP :: Parser Expr
atomicExprP = do
  expr <-
    parensP exprP
    <|> ArrayE . Vec.map Id . Vec.fromList
        <$> bracketsP (sepEndBy exprP (specialCharP ','))
    <|> LiteralE <$> numberP
    <|> NameE <$> nameP
  fields <- many (specialCharP '.' *> nameP)
  return (foldl' (\e f -> FieldAccessE f (Id e)) expr fields)
 where
  parensP = between (specialCharP '(') (specialCharP ')')
  bracketsP = between (specialCharP '[') (specialCharP ']')

numberP :: Parser Literal
numberP = NumberL <$> scientific <* space

-- TODO: parse other kinds of exprs!

specialCharP :: Char -> Parser ()
specialCharP c = single c >> space
