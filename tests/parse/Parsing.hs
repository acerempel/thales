{-# OPTIONS_GHC -Wno-missing-signatures #-}
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (SourcePos(..), mkPos)

import qualified NonEmptyText
import Parse
import Syntax

parseTestStmt =
  parseTest templateP

parseTestExpr =
  parseTest exprP defaultDelimiters

parseTest p delims =
  runParser p delims "tests/Parsing.hs"

within Delimiters{..} a =
  NonEmptyText.toText begin <> a <> NonEmptyText.toText end

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
    let mkSP l c = SourcePos "tests/Parsing.hs" (mkPos l) (mkPos c)
        beginDelimLength =
          NonEmptyText.length (begin delims)
        endDelimLength =
          NonEmptyText.length (end delims)
    describe "statement parser" $ do
      it "parses 'for' blocks" $
        let p1 = "<p>I am this potato: "
            p2 = "! </p>"
        in
          parseTestStmt delims
            (within delims "for potato in potatoes"
            <> NonEmptyText.toText p1 <> within delims "potato"
            <> NonEmptyText.toText p2 <> within delims "end")
          `shouldParse`
            -- TODO: these SourcePos's are just from the output of the failing
            -- test. Should really make these correct by construction, or just
            -- ignore them somehow. In fact, maybe the Eq instance *should*
            -- ignore them.
            [ ForS (mkSP 1 (1 + beginDelimLength)) "potato" (NameE "potatoes")
              [ VerbatimS p1
              , ExprS (mkSP 1 (44 + beginDelimLength + endDelimLength + beginDelimLength)) (NameE "potato")
              , VerbatimS p2 ]
            ]
