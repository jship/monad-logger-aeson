{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module TestCase.LogInfoNS.MetadataNoThreadContextYes
  ( testCase
  ) where

import Control.Monad.Logger.Aeson
  ( (.@), Loc(..), LogLevel(..), LoggedMessage(..), logInfoNS, withThreadContext
  )
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Time (UTCTime(..))
import TestCase (TestCase(..))
import qualified Control.Monad.Logger.Aeson.Internal as Internal
import qualified Data.Time as Time

testCase :: FilePath -> TestCase
testCase logFilePath =
  TestCase
    { actionUnderTest = do
        withThreadContext ["reqId" .@ ("74ec1d0b" :: String)] do
          logInfoNS "tests" "No metadata"
    , logFilePath
    , expectedValue =
        [aesonQQ|
          {
            "time": "2022-05-07T20:03:54.0000000Z",
            "level": "info",
            "location": {
              "package": "main",
              "module": "TestCase.LogInfoNS.MetadataNoThreadContextYes",
              "file": "test-suite/TestCase/LogInfoNS/MetadataNoThreadContextYes.hs",
              "line": 23,
              "char": 11
            },
            "source": "tests",
            "context": {
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
                , loc_module = "TestCase.LogInfoNS.MetadataNoThreadContextYes"
                , loc_filename = "test-suite/TestCase/LogInfoNS/MetadataNoThreadContextYes.hs"
                , loc_start = (23, 11)
                , loc_end = (0, 0)
                }
          , loggedMessageLogSource = Just "tests"
          , loggedMessageThreadContext =
              Internal.keyMapFromList
                [ "reqId" .@ ("74ec1d0b" :: String)
                ]
          , loggedMessageText = "No metadata"
          , loggedMessageMeta = Internal.emptyKeyMap
          }
    }
