{-# LANGUAGE BlockArguments #-}
module Test.Control.Monad.Logger.JSONSpec
  ( spec
  ) where

import Control.Monad.Logger.JSON ()
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Monad.Logger.Json" do
    it "works" do
      'a' `shouldBe` 'a'
