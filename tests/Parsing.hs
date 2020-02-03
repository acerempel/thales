{-# OPTIONS_GHC -Wno-missing-signatures #-}
import Test.Hspec
import Test.Hspec.Megaparsec

import NonEmptyText
import Parse
import Syntax
import Verbatim

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
        `shouldParse` LiteralE (NumberL 12.3)
      it "parses integer literals" $ do
        parseTestExpr "10"
        `shouldParse` LiteralE (NumberL 10)
    it "parses field accesses" $ do
      parseTestExpr "grim.zim.zam"
      `shouldParse`
      FieldAccessE "zam"
        (Id (FieldAccessE "zim"
          (Id (NameE "grim"))))

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
            [ ForS "potato" (NameE "potatoes")
              [ VerbatimS (preEscaped p1)
              , ExprS (NameE "potato")
              , VerbatimS (preEscaped p2) ]
            ]
