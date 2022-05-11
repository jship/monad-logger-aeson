{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module Test.Control.Monad.Logger.CallStack.JSONSpec
  ( spec
  ) where

import Control.Monad.Logger.CallStack.JSON
import Data.Aeson (Result(..), (.=), Value)
import Data.Aeson.Diff (Patch(..))
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Time (UTCTime(..))
import GHC.Stack (HasCallStack)
import System.IO (IOMode(..))
import Test.Hspec (Expectation, Spec, aroundAll, describe, expectationFailure, it, shouldBe)
import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Diff as Diff
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List as List
import qualified Data.Time as Time
import qualified System.Directory as Directory
import qualified System.IO as IO

spec :: Spec
spec = do
  aroundAll withTempLogFile do
    describe "Control.Monad.Logger.CallStack.JSON" do
      describe "logDebug" do
        it "no metadata + no thread context" \logFilePath -> do
          runTest TestCase
            { actionUnderTest = logDebug "No metadata"
            , logFilePath
            , expectedValue =
                [aesonQQ|
                  {
                    "time": "2022-05-07T20:03:54.0000000Z",
                    "level": "debug",
                    "location": {
                      "package": "main",
                      "module": "Test.Control.Monad.Logger.CallStack.JSONSpec",
                      "file": "test-suite/Test/Control/Monad/Logger/CallStack/JSONSpec.hs",
                      "line": 32,
                      "char": 31
                    },
                    "context": {
                      "tid": "ThreadId 1"
                    },
                    "message": {
                      "text": "No metadata"
                    }
                  }
                |]
            , expectedPatch =
                [aesonQQ|
                  [
                    { "op": "replace", "path": "/context/tid", "value": "ThreadId 1" },
                    { "op": "replace", "path": "/time", "value": "2022-05-07T20:03:54.0000000Z" }
                  ]
                |]
            , expectedLoggedMessage =
                LoggedMessage
                  { loggedMessageTimestamp =
                      UTCTime
                        { utctDay = Time.fromGregorian 2022 05 07
                        , utctDayTime = 72234
                        }
                  , loggedMessageLevel = LevelDebug
                  , loggedMessageLoc =
                      Just Loc
                        { loc_package = "main"
                        , loc_module = "Test.Control.Monad.Logger.CallStack.JSONSpec"
                        , loc_filename = "test-suite/Test/Control/Monad/Logger/CallStack/JSONSpec.hs"
                        , loc_start = (32, 31)
                        , loc_end = (0, 0)
                        }
                  , loggedMessageLogSource = Nothing
                  , loggedMessageThreadContext = ["tid" .= ("ThreadId 1" :: String)]
                  , loggedMessageMessage = "No metadata"
                  }
            }

        it "no metadata + thread context" \logFilePath -> do
          runTest TestCase
            { actionUnderTest =
                withThreadContext ["reqId" .= ("74ec1d0b" :: String)] do
                  logDebug $ "No metadata"
            , logFilePath
            , expectedValue =
                [aesonQQ|
                  {
                    "time": "2022-05-07T20:03:54.0000000Z",
                    "level": "debug",
                    "location": {
                      "package": "main",
                      "module": "Test.Control.Monad.Logger.CallStack.JSONSpec",
                      "file": "test-suite/Test/Control/Monad/Logger/CallStack/JSONSpec.hs",
                      "line": 86,
                      "char": 17
                    },
                    "context": {
                      "tid": "ThreadId 1",
                      "reqId": "74ec1d0b"
                    },
                    "message": {
                      "text": "No metadata"
                    }
                  }
                |]
            , expectedPatch =
                [aesonQQ|
                  [
                    { "op": "replace", "path": "/context/tid", "value": "ThreadId 1" },
                    { "op": "replace", "path": "/time", "value": "2022-05-07T20:03:54.0000000Z" }
                  ]
                |]
            , expectedLoggedMessage =
                LoggedMessage
                  { loggedMessageTimestamp =
                      UTCTime
                        { utctDay = Time.fromGregorian 2022 05 07
                        , utctDayTime = 72234
                        }
                  , loggedMessageLevel = LevelDebug
                  , loggedMessageLoc =
                      Just Loc
                        { loc_package = "main"
                        , loc_module = "Test.Control.Monad.Logger.CallStack.JSONSpec"
                        , loc_filename = "test-suite/Test/Control/Monad/Logger/CallStack/JSONSpec.hs"
                        , loc_start = (86, 17)
                        , loc_end = (0, 0)
                        }
                  , loggedMessageLogSource = Nothing
                  , loggedMessageThreadContext =
                      [ "reqId" .= ("74ec1d0b" :: String)
                      , "tid" .= ("ThreadId 1" :: String)
                      ]
                  , loggedMessageMessage = "No metadata"
                  }
            }

        it "metadata + no thread context" \logFilePath -> do
          runTest TestCase
            { actionUnderTest = logDebug $ "With metadata" :#
                [ "a" .= (42 :: Int)
                , "b" .= ("x" :: String)
                ]
            , logFilePath
            , expectedValue =
                [aesonQQ|
                  {
                    "time": "2022-05-07T20:03:54.0000000Z",
                    "level": "debug",
                    "location": {
                      "package": "main",
                      "module": "Test.Control.Monad.Logger.CallStack.JSONSpec",
                      "file": "test-suite/Test/Control/Monad/Logger/CallStack/JSONSpec.hs",
                      "line": 142,
                      "char": 31
                    },
                    "context": {
                      "tid": "ThreadId 1"
                    },
                    "message": {
                      "text": "With metadata",
                      "meta": {
                        "a": 42,
                        "b": "x"
                      }
                    }
                  }
                |]
            , expectedPatch =
                [aesonQQ|
                  [
                    { "op": "replace", "path": "/context/tid", "value": "ThreadId 1" },
                    { "op": "replace", "path": "/time", "value": "2022-05-07T20:03:54.0000000Z" }
                  ]
                |]
            , expectedLoggedMessage =
                LoggedMessage
                  { loggedMessageTimestamp =
                      UTCTime
                        { utctDay = Time.fromGregorian 2022 05 07
                        , utctDayTime = 72234
                        }
                  , loggedMessageLevel = LevelDebug
                  , loggedMessageLoc =
                      Just Loc
                        { loc_package = "main"
                        , loc_module = "Test.Control.Monad.Logger.CallStack.JSONSpec"
                        , loc_filename = "test-suite/Test/Control/Monad/Logger/CallStack/JSONSpec.hs"
                        , loc_start = (142, 31)
                        , loc_end = (0, 0)
                        }
                  , loggedMessageLogSource = Nothing
                  , loggedMessageThreadContext = ["tid" .= ("ThreadId 1" :: String)]
                  , loggedMessageMessage = "With metadata" :#
                      [ "a" .= (42 :: Int)
                      , "b" .= ("x" :: String)
                      ]
                  }
            }

        it "metadata + thread context" \logFilePath -> do
          runTest TestCase
            { actionUnderTest =
                withThreadContext ["reqId" .= ("74ec1d0b" :: String)] do
                  logDebug $ "With metadata" :#
                    [ "a" .= (42 :: Int)
                    , "b" .= ("x" :: String)
                    ]
            , logFilePath
            , expectedValue =
                [aesonQQ|
                  {
                    "time": "2022-05-07T20:03:54.0000000Z",
                    "level": "debug",
                    "location": {
                      "package": "main",
                      "module": "Test.Control.Monad.Logger.CallStack.JSONSpec",
                      "file": "test-suite/Test/Control/Monad/Logger/CallStack/JSONSpec.hs",
                      "line": 206,
                      "char": 17
                    },
                    "context": {
                      "tid": "ThreadId 1",
                      "reqId": "74ec1d0b"
                    },
                    "message": {
                      "text": "With metadata",
                      "meta": {
                        "a": 42,
                        "b": "x"
                      }
                    }
                  }
                |]
            , expectedPatch =
                [aesonQQ|
                  [
                    { "op": "replace", "path": "/context/tid", "value": "ThreadId 1" },
                    { "op": "replace", "path": "/time", "value": "2022-05-07T20:03:54.0000000Z" }
                  ]
                |]
            , expectedLoggedMessage =
                LoggedMessage
                  { loggedMessageTimestamp =
                      UTCTime
                        { utctDay = Time.fromGregorian 2022 05 07
                        , utctDayTime = 72234
                        }
                  , loggedMessageLevel = LevelDebug
                  , loggedMessageLoc =
                      Just Loc
                        { loc_package = "main"
                        , loc_module = "Test.Control.Monad.Logger.CallStack.JSONSpec"
                        , loc_filename = "test-suite/Test/Control/Monad/Logger/CallStack/JSONSpec.hs"
                        , loc_start = (206, 17)
                        , loc_end = (0, 0)
                        }
                  , loggedMessageLogSource = Nothing
                  , loggedMessageThreadContext =
                      [ "reqId" .= ("74ec1d0b" :: String)
                      , "tid" .= ("ThreadId 1" :: String)
                      ]
                  , loggedMessageMessage = "With metadata" :#
                      [ "a" .= (42 :: Int)
                      , "b" .= ("x" :: String)
                      ]
                  }
            }

data TestCase = TestCase
  { actionUnderTest :: LoggingT IO ()
  , logFilePath :: FilePath
  , expectedValue :: Value
  , expectedPatch :: Value
  , expectedLoggedMessage :: LoggedMessage
  }

withTempLogFile :: (FilePath -> IO ()) -> IO ()
withTempLogFile f = do
  Exception.bracket
    (IO.openTempFile "." "monad-logger-aeson")
    (\(filePath, handle) -> IO.hClose handle *> Directory.removeFile filePath)
    \(filePath, handle) -> do
      IO.hClose handle
      f filePath

clearFile :: FilePath -> IO ()
clearFile filePath = IO.withFile filePath WriteMode $ flip IO.hSetFileSize 0

runTest :: (HasCallStack) => TestCase -> IO ()
runTest testCase = do
  clearFile logFilePath
  runFileLoggingT logFilePath actionUnderTest
  curLine <- IO.withFile logFilePath ReadMode BS8.hGetLine
  case Aeson.decodeStrict' @Value curLine of
    Nothing -> expectationFailure $ "Failed to parse log line as Value: " <> show curLine
    Just actualValue -> do
      case Aeson.fromJSON expectedPatch of
        Error errStr -> do
          expectationFailure $ "Patch is invalid: " <> show errStr
        Success expectedPatch' -> do
          actualValue `shouldMatchWithPatch` (expectedValue, expectedPatch')
          Diff.patch expectedPatch' actualValue `shouldBe` Success expectedValue
          case Aeson.fromJSON expectedValue of
            Error errStr -> do
              expectationFailure $ "LoggedMessage parse failed: " <> errStr
            Success actualLoggedMessage -> do
              actualLoggedMessage `shouldMatchLoggedMessage` expectedLoggedMessage
  where
  TestCase
    { actionUnderTest
    , logFilePath
    , expectedValue
    , expectedPatch
    , expectedLoggedMessage
    } = testCase

shouldMatchWithPatch :: (HasCallStack) => Value -> (Value, Patch) -> Expectation
shouldMatchWithPatch actualValue (expectedValue, expectedPatch) = do
  Diff.diff actualValue expectedValue `shouldBe` expectedPatch

shouldMatchLoggedMessage
  :: (HasCallStack)
  => LoggedMessage
  -> LoggedMessage
  -> Expectation
shouldMatchLoggedMessage x y =
  sortPairs x `shouldBe` sortPairs y
  where
  sortPairs lm =
    lm
      { loggedMessageThreadContext = List.sort $ loggedMessageThreadContext lm
      , loggedMessageMessage =
          case loggedMessageMessage lm of
            messageText :# messageMeta -> messageText :# List.sort messageMeta
      }
