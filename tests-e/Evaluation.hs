{-# OPTIONS_GHC -Wno-missing-signatures #-}
import Test.Hspec

import qualified Data.HashMap.Strict as Map

import Eval
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
