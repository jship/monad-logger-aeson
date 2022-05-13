{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module TestCase.LogWarn.MetadataNoThreadContextNo
  ( testCase
  ) where

import Control.Monad.Logger.Aeson ((.@), Loc(..), LogLevel(..), LoggedMessage(..), logWarn)
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Time (UTCTime(..))
import TestCase (TestCase(..))
import qualified Data.Time as Time

testCase :: FilePath -> TestCase
testCase logFilePath =
  TestCase
    { actionUnderTest = do
        logWarn "No metadata"
    , logFilePath
    , expectedValue =
        [aesonQQ|
          {
            "time": "2022-05-07T20:03:54.0000000Z",
            "level": "warn",
            "location": {
              "package": "main",
              "module": "TestCase.LogWarn.MetadataNoThreadContextNo",
              "file": "test-suite/TestCase/LogWarn/MetadataNoThreadContextNo.hs",
              "line": 18,
              "char": 9
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
          , loggedMessageLevel = LevelWarn
          , loggedMessageLoc =
              Just Loc
                { loc_package = "main"
                , loc_module = "TestCase.LogWarn.MetadataNoThreadContextNo"
                , loc_filename = "test-suite/TestCase/LogWarn/MetadataNoThreadContextNo.hs"
                , loc_start = (18, 9)
                , loc_end = (0, 0)
                }
          , loggedMessageLogSource = Nothing
          , loggedMessageThreadContext = ["tid" .@ ("ThreadId 1" :: String)]
          , loggedMessageMessage = "No metadata"
          }
    }
