{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module TestCase.LogInfoNS.MetadataNoThreadContextNo
  ( testCase
  ) where

import Control.Monad.Logger.Aeson (Loc(..), LogLevel(..), LoggedMessage(..), logInfoNS)
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Time (UTCTime(..))
import TestCase (TestCase(..))
import qualified Data.Time as Time

testCase :: FilePath -> TestCase
testCase logFilePath =
  TestCase
    { actionUnderTest = do
        logInfoNS "tests" "No metadata"
    , logFilePath
    , expectedValue =
        [aesonQQ|
          {
            "time": "2022-05-07T20:03:54.0000000Z",
            "level": "info",
            "location": {
              "package": "main",
              "module": "TestCase.LogInfoNS.MetadataNoThreadContextNo",
              "file": "test-suite/TestCase/LogInfoNS/MetadataNoThreadContextNo.hs",
              "line": 18,
              "char": 9
            },
            "source": "tests",
            "message": {
              "text": "No metadata"
            }
          }
        |]
    , expectedPatch =
        [aesonQQ|
          [
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
          , loggedMessageLevel = LevelInfo
          , loggedMessageLoc =
              Just Loc
                { loc_package = "main"
                , loc_module = "TestCase.LogInfoNS.MetadataNoThreadContextNo"
                , loc_filename = "test-suite/TestCase/LogInfoNS/MetadataNoThreadContextNo.hs"
                , loc_start = (18, 9)
                , loc_end = (0, 0)
                }
          , loggedMessageLogSource = Just "tests"
          , loggedMessageThreadContext = []
          , loggedMessageMessage = "No metadata"
          }
    }
