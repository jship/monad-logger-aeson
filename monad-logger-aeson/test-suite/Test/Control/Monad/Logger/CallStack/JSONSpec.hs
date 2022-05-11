{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module Test.Control.Monad.Logger.CallStack.JSONSpec
  ( spec
  ) where

import Test.Hspec (Spec, aroundAll, describe, it)
import TestCase (runTest, withTempLogFile)
import qualified TestCase.LogDebug.MetadataNoThreadContextNo
import qualified TestCase.LogDebug.MetadataNoThreadContextYes
import qualified TestCase.LogDebug.MetadataYesThreadContextNo
import qualified TestCase.LogDebug.MetadataYesThreadContextYes

spec :: Spec
spec = do
  aroundAll withTempLogFile do
    describe "Control.Monad.Logger.CallStack.JSON" do
      describe "logDebug" do
        it "no metadata + no thread context" \logFilePath -> do
          runTest $ TestCase.LogDebug.MetadataNoThreadContextNo.testCase logFilePath
        it "no metadata + thread context" \logFilePath -> do
          runTest $ TestCase.LogDebug.MetadataNoThreadContextYes.testCase logFilePath
        it "metadata + no thread context" \logFilePath -> do
          runTest $ TestCase.LogDebug.MetadataYesThreadContextNo.testCase logFilePath
        it "metadata + thread context" \logFilePath -> do
          runTest $ TestCase.LogDebug.MetadataYesThreadContextYes.testCase logFilePath
