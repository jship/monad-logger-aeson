{-# LANGUAGE BlockArguments #-}
module Test.Control.Monad.Logger.CallStack.JSONSpec
  ( spec
  ) where

import Control.Monad.Logger.CallStack.JSON ()
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Control.Monad.Logger.CallStack.JSON" do
    it "works" do
      'a' `shouldBe` 'a'
