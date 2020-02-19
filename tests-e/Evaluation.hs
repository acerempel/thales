{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
import Test.Hspec

import qualified Data.HashMap.Strict as Map
import Text.Megaparsec (SourcePos(..), mkPos)

import Eval
import qualified List
import Syntax
import Value

main = hspec $ do
  describe "Evaluation" $ do
    it "evaluates names to their bindings" $ do
      let expr = NameE "potato"
          bindings = Map.singleton "potato" (Number 3)
      result <- runEvalM (evalTopExpr expr) bindings
      result `shouldBe` Right (Number 3)
    it "evaluates field accesses" $ do
      let expr = FieldAccessE "potato" (Id (NameE "vegetables"))
          bindings =
            Map.singleton "vegetables"
            (Record (Map.singleton "potato" (Number 7)))
      result <- runEvalM (evalTopExpr expr) bindings
      result `shouldBe` Right (Number 7)
    it "evaluates function calls" $ do
      let expr =
            ApplyE (Id (NameE "plus-one")) (Id (LiteralE (NumberL 4)))
          bindings =
            Map.singleton "plus-one"
            (Function NumberT (return . Number . (+1)))
      result <- runEvalM (evalTopExpr expr) bindings
      result `shouldBe` Right (Number 5)
    it "evaluates for statements" $ do
      let sp = SourcePos "tests-e/Evaluation.hs" (mkPos 1) (mkPos 1)
          stmt =
            ForS sp "potato"
              (ArrayE (List.map (Id . LiteralE . NumberL) [1,3,5,7]))
              [ExprS sp (NameE "potato")]
      result <- runEvalM (evalStatement stmt) Map.empty
      result `shouldBe` Right (List.map Number [1,3,5,7])
