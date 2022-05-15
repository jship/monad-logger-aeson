{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module TestCase.LogOtherNS.MetadataNoThreadContextNo
  ( testCase
  ) where

import Control.Monad.Logger.Aeson (Loc(..), LogLevel(..), LoggedMessage(..), logOtherNS)
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Time (UTCTime(..))
import TestCase (TestCase(..))
import qualified Data.Time as Time

testCase :: FilePath -> TestCase
testCase logFilePath =
  TestCase
    { actionUnderTest = do
        logOtherNS "tests" (LevelOther "foo") "No metadata"
    , logFilePath
    , expectedValue =
        [aesonQQ|
          {
            "time": "2022-05-07T20:03:54.0000000Z",
            "level": "foo",
            "location": {
              "package": "main",
              "module": "TestCase.LogOtherNS.MetadataNoThreadContextNo",
              "file": "test-suite/TestCase/LogOtherNS/MetadataNoThreadContextNo.hs",
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
          , loggedMessageLevel = LevelOther "foo"
          , loggedMessageLoc =
              Just Loc
                { loc_package = "main"
                , loc_module = "TestCase.LogOtherNS.MetadataNoThreadContextNo"
                , loc_filename = "test-suite/TestCase/LogOtherNS/MetadataNoThreadContextNo.hs"
                , loc_start = (18, 9)
                , loc_end = (0, 0)
                }
          , loggedMessageLogSource = Just "tests"
          , loggedMessageThreadContext = []
          , loggedMessageMessage = "No metadata"
          }
    }
