{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module TestCase.LogDebugNS.MetadataNoThreadContextNo
  ( testCase
  ) where

import Control.Monad.Logger.CallStack.JSON (Loc(..), LogLevel(..), LoggedMessage(..), logDebugNS)
import Data.Aeson ((.=))
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Time (UTCTime(..))
import TestCase (TestCase(..))
import qualified Data.Time as Time

testCase :: FilePath -> TestCase
testCase logFilePath =
  TestCase
    { actionUnderTest = do
        logDebugNS "tests" "No metadata"
    , logFilePath
    , expectedValue =
        [aesonQQ|
          {
            "time": "2022-05-07T20:03:54.0000000Z",
            "level": "debug",
            "location": {
              "package": "main",
              "module": "TestCase.LogDebugNS.MetadataNoThreadContextNo",
              "file": "test-suite/TestCase/LogDebugNS/MetadataNoThreadContextNo.hs",
              "line": 19,
              "char": 9
            },
            "source": "tests",
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
                , loc_module = "TestCase.LogDebugNS.MetadataNoThreadContextNo"
                , loc_filename = "test-suite/TestCase/LogDebugNS/MetadataNoThreadContextNo.hs"
                , loc_start = (19, 9)
                , loc_end = (0, 0)
                }
          , loggedMessageLogSource = Just "tests"
          , loggedMessageThreadContext = ["tid" .= ("ThreadId 1" :: String)]
          , loggedMessageMessage = "No metadata"
          }
    }
