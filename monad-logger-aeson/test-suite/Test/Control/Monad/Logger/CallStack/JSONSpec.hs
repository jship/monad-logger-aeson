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
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Diff as Diff
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Maybe as Maybe
import qualified Data.Time as Time
import qualified System.IO as IO
import qualified System.IO.Temp as IO.Temp

spec :: Spec
spec = do
  describe "Control.Monad.Logger.CallStack.JSON" do
    describe "logDebug" do
      it "no metadata + no thread context" do
        runTest TestCase
          { actionUnderTest = logDebug "No metadata"
          , expectedValue =
              [aesonQQ|
                {
                  "time": "2022-05-07T20:03:54.0000000Z",
                  "level": "debug",
                  "location": {
                    "package": "main",
                    "module": "Test.Control.Monad.Logger.CallStack.JSONSpec",
                    "file": "test-suite/Test/Control/Monad/Logger/CallStack/JSONSpec.hs",
                    "line": 31,
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
                      , loc_start = (31, 31)
                      , loc_end = (0, 0)
                      }
                , loggedMessageLogSource = Nothing
                , loggedMessageThreadContext = ["tid" .= ("ThreadId 1" :: String)]
                , loggedMessageMessage = "No metadata"
                }
          }

      it "no metadata + thread context" do
        runTest TestCase
          { actionUnderTest =
              withThreadContext ["reqId" .= ("74ec1d0b" :: String)] do
                logDebug $ "No metadata"
          , expectedValue =
              [aesonQQ|
                {
                  "time": "2022-05-07T20:03:54.0000000Z",
                  "level": "debug",
                  "location": {
                    "package": "main",
                    "module": "Test.Control.Monad.Logger.CallStack.JSONSpec",
                    "file": "test-suite/Test/Control/Monad/Logger/CallStack/JSONSpec.hs",
                    "line": 85,
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
                      , loc_start = (85, 17)
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

      it "metadata + no thread context" do
        runTest TestCase
          { actionUnderTest = logDebug $ "With metadata" :#
              [ "a" .= (42 :: Int)
              , "b" .= ("x" :: String)
              ]
          , expectedValue =
              [aesonQQ|
                {
                  "time": "2022-05-07T20:03:54.0000000Z",
                  "level": "debug",
                  "location": {
                    "package": "main",
                    "module": "Test.Control.Monad.Logger.CallStack.JSONSpec",
                    "file": "test-suite/Test/Control/Monad/Logger/CallStack/JSONSpec.hs",
                    "line": 141,
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
                      , loc_start = (141, 31)
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

      it "metadata + thread context" do
        runTest TestCase
          { actionUnderTest =
              withThreadContext ["reqId" .= ("74ec1d0b" :: String)] do
                logDebug $ "With metadata" :#
                  [ "a" .= (42 :: Int)
                  , "b" .= ("x" :: String)
                  ]
          , expectedValue =
              [aesonQQ|
                {
                  "time": "2022-05-07T20:03:54.0000000Z",
                  "level": "debug",
                  "location": {
                    "package": "main",
                    "module": "Test.Control.Monad.Logger.CallStack.JSONSpec",
                    "file": "test-suite/Test/Control/Monad/Logger/CallStack/JSONSpec.hs",
                    "line": 205,
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
                      , loc_start = (205, 17)
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
  , expectedValue :: Value
  , expectedPatch :: Value
  , expectedLoggedMessage :: LoggedMessage
  }

runTest :: (HasCallStack) => TestCase -> IO ()
runTest testCase = do
  IO.Temp.withSystemTempFile "monad-logger-aeson" \filePath handle -> do
    IO.hClose handle
    runFileLoggingT filePath actionUnderTest
    fileLines <- fmap BS8.lines $ BS8.readFile filePath
    case Maybe.catMaybes $ fmap (Aeson.decodeStrict' @Value) fileLines of
      [] -> expectationFailure "Parsed no logged values"
      [actualValue] -> do
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
                actualLoggedMessage `shouldBe` expectedLoggedMessage
      values -> do
        expectationFailure $ "Parsed too many logged values: " <> show values
  where
  TestCase
    { actionUnderTest
    , expectedValue
    , expectedPatch
    , expectedLoggedMessage
    } = testCase

shouldMatchWithPatch :: (HasCallStack) => Value -> (Value, Patch) -> Expectation
shouldMatchWithPatch actualValue (expectedValue, expectedPatch) = do
  Diff.diff actualValue expectedValue `shouldBe` expectedPatch
