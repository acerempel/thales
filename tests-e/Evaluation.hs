{-# OPTIONS_GHC -Wno-missing-signatures #-}
import Test.Hspec

import qualified Data.HashMap.Strict as Map

import Eval
import Syntax
import Value

main = hspec $ do
  describe "Evaluation" $ do
    it "evluates names to their bindings" $ do
      let expr = NameE "potato"
          bindings = Map.singleton "potato" (Number 3)
      result <- runEvalM (evalTopExpr expr) bindings
      result `shouldBe` Right (Number 3)
