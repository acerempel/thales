{-# OPTIONS_GHC -Wno-missing-signatures #-}
import Data.Text.Lazy.Builder
import Test.Hspec
import Test.Hspec.Megaparsec

import NonEmptyText
import Parse
import Syntax

parseTestStmt =
  parseTest templateP

parseTestExpr =
  parseTest exprP defaultDelimiters

parseTest p delims =
  runParser p delims "tests/Parsing.hs"

within Delimiters{..} a =
  fromNonEmptyText begin <> a <> fromNonEmptyText end

delimSets =
  [ Delimiters "{" "}"
  , Delimiters "{{" "}}"
  , Delimiters "${" "}"
  , Delimiters "[|" "|]"
  , Delimiters "|" "|"]

main = hspec $ do
  describe "Parser" $ do
    testExprParser
    foldl' (>>) (return ()) (map testStmtParser delimSets)

testExprParser =
  describe "expression parser" $ do
    describe "number literals" $ do
      it "parses decimal literals" $ do
        parseTestExpr "12.3"
        `shouldParse` Expr (LiteralE (NumberL 12.3))
      it "parses integer literals" $ do
        parseTestExpr "10"
        `shouldParse` Expr (LiteralE (NumberL 10))
    it "parses field accesses" $ do
      parseTestExpr "grim.zim.zam"
      `shouldParse`
      Expr (FieldAccessE "zam"
            (Expr (FieldAccessE "zim"
                   (Expr (NameE "grim")))))

testStmtParser delims =
  describe ("with delimiters " <> show delims) $ do
    describe "statement parser" $ do
      it "parses 'for' blocks" $
        let p1 = "<p>I am this potato: "
            p2 = "! </p>"
        in
          parseTestStmt delims
            (within delims "for potato in potatoes"
            <> p1 <> within delims "potato"
            <> p2 <> within delims "end")
          `shouldParse`
            [ ForS "potato" (Expr (NameE "potatoes"))
              [ VerbatimS (fromText p1)
              , ExprS (Expr (NameE "potato"))
              , VerbatimS (fromText p2) ]
            ]
