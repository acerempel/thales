{-# OPTIONS_GHC -Wno-missing-signatures #-}
import Test.Hspec
import Test.Hspec.Megaparsec

import NonEmptyText
import Parse
import Syntax

parseTest delims =
  parseTemplate delims "tests/Parsing.hs"

withinDelims Delimiters{..} a =
  fromNonEmptyText begin <> a <> fromNonEmptyText end

delimSets =
  [ Delimiters "{" "}"
  , Delimiters "{{" "}}"
  , Delimiters "${" "}"
  , Delimiters "[|" "|]"
  , Delimiters "|" "|"]

main = hspec $ do
  describe "Parser" $ do
    foldl' (>>) (return ()) (map testNumbers delimSets)

testNumbers delims =
  describe ("with delimiters " <> show delims) $ do
    describe "number literals" $ do
      it "parses decimal literals" $ do
        parseTest delims (withinDelims delims "12.3")
        `shouldParse` [ExprS (Expr (LiteralE (NumberL 12.3)))]
      it "parses integer literals" $ do
        parseTest delims (withinDelims delims "10")
        `shouldParse` [ExprS (Expr (LiteralE (NumberL 10)))]
