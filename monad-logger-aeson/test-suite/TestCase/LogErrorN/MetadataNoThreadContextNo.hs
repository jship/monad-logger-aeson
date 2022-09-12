{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module TestCase.LogErrorN.MetadataNoThreadContextNo
  ( testCase
  ) where

import Control.Monad.Logger.Aeson (Loc(..), LogLevel(..), LoggedMessage(..), logErrorN)
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Time (UTCTime(..))
import TestCase (TestCase(..))
import qualified Control.Monad.Logger.Aeson.Internal as Internal
import qualified Data.Time as Time

testCase :: FilePath -> TestCase
testCase logFilePath =
  TestCase
    { actionUnderTest = do
        logErrorN "No metadata"
    , logFilePath
    , expectedValue =
        [aesonQQ|
          {
            "time": "2022-05-07T20:03:54.0000000Z",
            "level": "error",
            "location": {
              "package": "main",
              "module": "TestCase.LogErrorN.MetadataNoThreadContextNo",
              "file": "test-suite/TestCase/LogErrorN/MetadataNoThreadContextNo.hs",
              "line": 19,
              "char": 9
            },
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
          , loggedMessageLevel = LevelError
          , loggedMessageLoc =
              Just Loc
                { loc_package = "main"
                , loc_module = "TestCase.LogErrorN.MetadataNoThreadContextNo"
                , loc_filename = "test-suite/TestCase/LogErrorN/MetadataNoThreadContextNo.hs"
                , loc_start = (19, 9)
                , loc_end = (0, 0)
                }
          , loggedMessageLogSource = Nothing
          , loggedMessageThreadContext = Internal.emptyKeyMap
          , loggedMessageText = "No metadata"
          , loggedMessageMeta = Internal.emptyKeyMap
          }
    }
