{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module Test.Control.Monad.Logger.AesonSpec
  ( spec
  ) where

import Test.Hspec (Spec, aroundAll, describe, it)
import TestCase (runTest, withTempLogFile)
import qualified TestCase.LogDebug.MetadataNoThreadContextNo
import qualified TestCase.LogDebug.MetadataNoThreadContextYes
import qualified TestCase.LogDebug.MetadataYesThreadContextNo
import qualified TestCase.LogDebug.MetadataYesThreadContextYes
import qualified TestCase.LogDebugN.MetadataNoThreadContextNo
import qualified TestCase.LogDebugN.MetadataNoThreadContextYes
import qualified TestCase.LogDebugN.MetadataYesThreadContextNo
import qualified TestCase.LogDebugN.MetadataYesThreadContextYes
import qualified TestCase.LogDebugNS.MetadataNoThreadContextNo
import qualified TestCase.LogDebugNS.MetadataNoThreadContextYes
import qualified TestCase.LogDebugNS.MetadataYesThreadContextNo
import qualified TestCase.LogDebugNS.MetadataYesThreadContextYes
import qualified TestCase.LogError.MetadataNoThreadContextNo
import qualified TestCase.LogError.MetadataNoThreadContextYes
import qualified TestCase.LogError.MetadataYesThreadContextNo
import qualified TestCase.LogError.MetadataYesThreadContextYes
import qualified TestCase.LogErrorN.MetadataNoThreadContextNo
import qualified TestCase.LogErrorN.MetadataNoThreadContextYes
import qualified TestCase.LogErrorN.MetadataYesThreadContextNo
import qualified TestCase.LogErrorN.MetadataYesThreadContextYes
import qualified TestCase.LogErrorNS.MetadataNoThreadContextNo
import qualified TestCase.LogErrorNS.MetadataNoThreadContextYes
import qualified TestCase.LogErrorNS.MetadataYesThreadContextNo
import qualified TestCase.LogErrorNS.MetadataYesThreadContextYes
import qualified TestCase.LogInfo.MetadataNoThreadContextNo
import qualified TestCase.LogInfo.MetadataNoThreadContextYes
import qualified TestCase.LogInfo.MetadataYesThreadContextNo
import qualified TestCase.LogInfo.MetadataYesThreadContextYes
import qualified TestCase.LogInfoN.MetadataNoThreadContextNo
import qualified TestCase.LogInfoN.MetadataNoThreadContextYes
import qualified TestCase.LogInfoN.MetadataYesThreadContextNo
import qualified TestCase.LogInfoN.MetadataYesThreadContextYes
import qualified TestCase.LogInfoNS.MetadataNoThreadContextNo
import qualified TestCase.LogInfoNS.MetadataNoThreadContextYes
import qualified TestCase.LogInfoNS.MetadataYesThreadContextNo
import qualified TestCase.LogInfoNS.MetadataYesThreadContextYes
import qualified TestCase.LogOther.MetadataNoThreadContextNo
import qualified TestCase.LogOther.MetadataNoThreadContextYes
import qualified TestCase.LogOther.MetadataYesThreadContextNo
import qualified TestCase.LogOther.MetadataYesThreadContextYes
import qualified TestCase.LogOtherN.MetadataNoThreadContextNo
import qualified TestCase.LogOtherN.MetadataNoThreadContextYes
import qualified TestCase.LogOtherN.MetadataYesThreadContextNo
import qualified TestCase.LogOtherN.MetadataYesThreadContextYes
import qualified TestCase.LogOtherNS.MetadataNoThreadContextNo
import qualified TestCase.LogOtherNS.MetadataNoThreadContextYes
import qualified TestCase.LogOtherNS.MetadataYesThreadContextNo
import qualified TestCase.LogOtherNS.MetadataYesThreadContextYes
import qualified TestCase.LogWarn.MetadataNoThreadContextNo
import qualified TestCase.LogWarn.MetadataNoThreadContextYes
import qualified TestCase.LogWarn.MetadataYesThreadContextNo
import qualified TestCase.LogWarn.MetadataYesThreadContextYes
import qualified TestCase.LogWarnN.MetadataNoThreadContextNo
import qualified TestCase.LogWarnN.MetadataNoThreadContextYes
import qualified TestCase.LogWarnN.MetadataYesThreadContextNo
import qualified TestCase.LogWarnN.MetadataYesThreadContextYes
import qualified TestCase.LogWarnNS.MetadataNoThreadContextNo
import qualified TestCase.LogWarnNS.MetadataNoThreadContextYes
import qualified TestCase.LogWarnNS.MetadataYesThreadContextNo
import qualified TestCase.LogWarnNS.MetadataYesThreadContextYes
import qualified TestCase.MonadLogger.LogDebug.ThreadContextNo
import qualified TestCase.MonadLogger.LogDebug.ThreadContextYes
import qualified TestCase.MonadLogger.LogDebugN.ThreadContextNo
import qualified TestCase.MonadLogger.LogDebugN.ThreadContextYes

spec :: Spec
spec = do
  aroundAll withTempLogFile $ do
    describe "Control.Monad.Logger.Aeson" $ do
      describe "logDebug" $ do
        it "no metadata + no thread context" $ \logFilePath -> do
          runTest $ TestCase.LogDebug.MetadataNoThreadContextNo.testCase logFilePath
        it "no metadata + thread context" $ \logFilePath -> do
          runTest $ TestCase.LogDebug.MetadataNoThreadContextYes.testCase logFilePath
        it "metadata + no thread context" $ \logFilePath -> do
          runTest $ TestCase.LogDebug.MetadataYesThreadContextNo.testCase logFilePath
        it "metadata + thread context" $ \logFilePath -> do
          runTest $ TestCase.LogDebug.MetadataYesThreadContextYes.testCase logFilePath

      describe "logInfo" $ do
        it "no metadata + no thread context" $ \logFilePath -> do
          runTest $ TestCase.LogInfo.MetadataNoThreadContextNo.testCase logFilePath
        it "no metadata + thread context" $ \logFilePath -> do
          runTest $ TestCase.LogInfo.MetadataNoThreadContextYes.testCase logFilePath
        it "metadata + no thread context" $ \logFilePath -> do
          runTest $ TestCase.LogInfo.MetadataYesThreadContextNo.testCase logFilePath
        it "metadata + thread context" $ \logFilePath -> do
          runTest $ TestCase.LogInfo.MetadataYesThreadContextYes.testCase logFilePath

      describe "logWarn" $ do
        it "no metadata + no thread context" $ \logFilePath -> do
          runTest $ TestCase.LogWarn.MetadataNoThreadContextNo.testCase logFilePath
        it "no metadata + thread context" $ \logFilePath -> do
          runTest $ TestCase.LogWarn.MetadataNoThreadContextYes.testCase logFilePath
        it "metadata + no thread context" $ \logFilePath -> do
          runTest $ TestCase.LogWarn.MetadataYesThreadContextNo.testCase logFilePath
        it "metadata + thread context" $ \logFilePath -> do
          runTest $ TestCase.LogWarn.MetadataYesThreadContextYes.testCase logFilePath

      describe "logError" $ do
        it "no metadata + no thread context" $ \logFilePath -> do
          runTest $ TestCase.LogError.MetadataNoThreadContextNo.testCase logFilePath
        it "no metadata + thread context" $ \logFilePath -> do
          runTest $ TestCase.LogError.MetadataNoThreadContextYes.testCase logFilePath
        it "metadata + no thread context" $ \logFilePath -> do
          runTest $ TestCase.LogError.MetadataYesThreadContextNo.testCase logFilePath
        it "metadata + thread context" $ \logFilePath -> do
          runTest $ TestCase.LogError.MetadataYesThreadContextYes.testCase logFilePath

      describe "logOther" $ do
        it "no metadata + no thread context" $ \logFilePath -> do
          runTest $ TestCase.LogOther.MetadataNoThreadContextNo.testCase logFilePath
        it "no metadata + thread context" $ \logFilePath -> do
          runTest $ TestCase.LogOther.MetadataNoThreadContextYes.testCase logFilePath
        it "metadata + no thread context" $ \logFilePath -> do
          runTest $ TestCase.LogOther.MetadataYesThreadContextNo.testCase logFilePath
        it "metadata + thread context" $ \logFilePath -> do
          runTest $ TestCase.LogOther.MetadataYesThreadContextYes.testCase logFilePath

      describe "logDebugN" $ do
        it "no metadata + no thread context" $ \logFilePath -> do
          runTest $ TestCase.LogDebugN.MetadataNoThreadContextNo.testCase logFilePath
        it "no metadata + thread context" $ \logFilePath -> do
          runTest $ TestCase.LogDebugN.MetadataNoThreadContextYes.testCase logFilePath
        it "metadata + no thread context" $ \logFilePath -> do
          runTest $ TestCase.LogDebugN.MetadataYesThreadContextNo.testCase logFilePath
        it "metadata + thread context" $ \logFilePath -> do
          runTest $ TestCase.LogDebugN.MetadataYesThreadContextYes.testCase logFilePath

      describe "logInfoN" $ do
        it "no metadata + no thread context" $ \logFilePath -> do
          runTest $ TestCase.LogInfoN.MetadataNoThreadContextNo.testCase logFilePath
        it "no metadata + thread context" $ \logFilePath -> do
          runTest $ TestCase.LogInfoN.MetadataNoThreadContextYes.testCase logFilePath
        it "metadata + no thread context" $ \logFilePath -> do
          runTest $ TestCase.LogInfoN.MetadataYesThreadContextNo.testCase logFilePath
        it "metadata + thread context" $ \logFilePath -> do
          runTest $ TestCase.LogInfoN.MetadataYesThreadContextYes.testCase logFilePath

      describe "logWarnN" $ do
        it "no metadata + no thread context" $ \logFilePath -> do
          runTest $ TestCase.LogWarnN.MetadataNoThreadContextNo.testCase logFilePath
        it "no metadata + thread context" $ \logFilePath -> do
          runTest $ TestCase.LogWarnN.MetadataNoThreadContextYes.testCase logFilePath
        it "metadata + no thread context" $ \logFilePath -> do
          runTest $ TestCase.LogWarnN.MetadataYesThreadContextNo.testCase logFilePath
        it "metadata + thread context" $ \logFilePath -> do
          runTest $ TestCase.LogWarnN.MetadataYesThreadContextYes.testCase logFilePath

      describe "logErrorN" $ do
        it "no metadata + no thread context" $ \logFilePath -> do
          runTest $ TestCase.LogErrorN.MetadataNoThreadContextNo.testCase logFilePath
        it "no metadata + thread context" $ \logFilePath -> do
          runTest $ TestCase.LogErrorN.MetadataNoThreadContextYes.testCase logFilePath
        it "metadata + no thread context" $ \logFilePath -> do
          runTest $ TestCase.LogErrorN.MetadataYesThreadContextNo.testCase logFilePath
        it "metadata + thread context" $ \logFilePath -> do
          runTest $ TestCase.LogErrorN.MetadataYesThreadContextYes.testCase logFilePath

      describe "logOtherN" $ do
        it "no metadata + no thread context" $ \logFilePath -> do
          runTest $ TestCase.LogOtherN.MetadataNoThreadContextNo.testCase logFilePath
        it "no metadata + thread context" $ \logFilePath -> do
          runTest $ TestCase.LogOtherN.MetadataNoThreadContextYes.testCase logFilePath
        it "metadata + no thread context" $ \logFilePath -> do
          runTest $ TestCase.LogOtherN.MetadataYesThreadContextNo.testCase logFilePath
        it "metadata + thread context" $ \logFilePath -> do
          runTest $ TestCase.LogOtherN.MetadataYesThreadContextYes.testCase logFilePath

      describe "logDebugNS" $ do
        it "no metadata + no thread context" $ \logFilePath -> do
          runTest $ TestCase.LogDebugNS.MetadataNoThreadContextNo.testCase logFilePath
        it "no metadata + thread context" $ \logFilePath -> do
          runTest $ TestCase.LogDebugNS.MetadataNoThreadContextYes.testCase logFilePath
        it "metadata + no thread context" $ \logFilePath -> do
          runTest $ TestCase.LogDebugNS.MetadataYesThreadContextNo.testCase logFilePath
        it "metadata + thread context" $ \logFilePath -> do
          runTest $ TestCase.LogDebugNS.MetadataYesThreadContextYes.testCase logFilePath

      describe "logInfoNS" $ do
        it "no metadata + no thread context" $ \logFilePath -> do
          runTest $ TestCase.LogInfoNS.MetadataNoThreadContextNo.testCase logFilePath
        it "no metadata + thread context" $ \logFilePath -> do
          runTest $ TestCase.LogInfoNS.MetadataNoThreadContextYes.testCase logFilePath
        it "metadata + no thread context" $ \logFilePath -> do
          runTest $ TestCase.LogInfoNS.MetadataYesThreadContextNo.testCase logFilePath
        it "metadata + thread context" $ \logFilePath -> do
          runTest $ TestCase.LogInfoNS.MetadataYesThreadContextYes.testCase logFilePath

      describe "logWarnNS" $ do
        it "no metadata + no thread context" $ \logFilePath -> do
          runTest $ TestCase.LogWarnNS.MetadataNoThreadContextNo.testCase logFilePath
        it "no metadata + thread context" $ \logFilePath -> do
          runTest $ TestCase.LogWarnNS.MetadataNoThreadContextYes.testCase logFilePath
        it "metadata + no thread context" $ \logFilePath -> do
          runTest $ TestCase.LogWarnNS.MetadataYesThreadContextNo.testCase logFilePath
        it "metadata + thread context" $ \logFilePath -> do
          runTest $ TestCase.LogWarnNS.MetadataYesThreadContextYes.testCase logFilePath

      describe "logErrorNS" $ do
        it "no metadata + no thread context" $ \logFilePath -> do
          runTest $ TestCase.LogErrorNS.MetadataNoThreadContextNo.testCase logFilePath
        it "no metadata + thread context" $ \logFilePath -> do
          runTest $ TestCase.LogErrorNS.MetadataNoThreadContextYes.testCase logFilePath
        it "metadata + no thread context" $ \logFilePath -> do
          runTest $ TestCase.LogErrorNS.MetadataYesThreadContextNo.testCase logFilePath
        it "metadata + thread context" $ \logFilePath -> do
          runTest $ TestCase.LogErrorNS.MetadataYesThreadContextYes.testCase logFilePath

      describe "logOtherNS" $ do
        it "no metadata + no thread context" $ \logFilePath -> do
          runTest $ TestCase.LogOtherNS.MetadataNoThreadContextNo.testCase logFilePath
        it "no metadata + thread context" $ \logFilePath -> do
          runTest $ TestCase.LogOtherNS.MetadataNoThreadContextYes.testCase logFilePath
        it "metadata + no thread context" $ \logFilePath -> do
          runTest $ TestCase.LogOtherNS.MetadataYesThreadContextNo.testCase logFilePath
        it "metadata + thread context" $ \logFilePath -> do
          runTest $ TestCase.LogOtherNS.MetadataYesThreadContextYes.testCase logFilePath

    describe "Control.Monad.Logger.CallStack ('log*' from 'monad-logger')" $ do
      describe "logDebug" $ do
        it "no metadata + no thread context" $ \logFilePath -> do
          runTest $ TestCase.MonadLogger.LogDebug.ThreadContextNo.testCase logFilePath
        it "no metadata + thread context" $ \logFilePath -> do
          runTest $ TestCase.MonadLogger.LogDebug.ThreadContextYes.testCase logFilePath

      describe "logDebugN" $ do
        it "no metadata + no thread context" $ \logFilePath -> do
          runTest $ TestCase.MonadLogger.LogDebugN.ThreadContextNo.testCase logFilePath
        it "no metadata + thread context" $ \logFilePath -> do
          runTest $ TestCase.MonadLogger.LogDebugN.ThreadContextYes.testCase logFilePath
