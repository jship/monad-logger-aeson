{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module TestCase.MonadLogger.LogDebug.ThreadContextYes
  ( testCase
  ) where

import Control.Monad.Logger.Aeson ((.@), Loc(..), LogLevel(..), LoggedMessage(..), withThreadContext)
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Time (UTCTime(..))
import TestCase (TestCase(..))
import qualified Control.Monad.Logger.CallStack as ML
import qualified Data.Time as Time

testCase :: FilePath -> TestCase
testCase logFilePath =
  TestCase
    { actionUnderTest = do
        withThreadContext ["reqId" .@ ("74ec1d0b" :: String)] do
          ML.logDebug "Logged from 'monad-logger'"
    , logFilePath
    , expectedValue =
        [aesonQQ|
          {
            "time": "2022-05-07T20:03:54.0000000Z",
            "level": "debug",
            "location": {
              "package": "main",
              "module": "TestCase.MonadLogger.LogDebug.ThreadContextYes",
              "file": "test-suite/TestCase/MonadLogger/LogDebug/ThreadContextYes.hs",
              "line": 21,
              "char": 11
            },
            "context": {
              "tid": "ThreadId 1",
              "reqId": "74ec1d0b"
            },
            "message": {
              "text": "Logged from 'monad-logger'"
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
                , loc_module = "TestCase.MonadLogger.LogDebug.ThreadContextYes"
                , loc_filename = "test-suite/TestCase/MonadLogger/LogDebug/ThreadContextYes.hs"
                , loc_start = (21, 11)
                , loc_end = (0, 0)
                }
          , loggedMessageLogSource = Nothing
          , loggedMessageThreadContext =
              [ "reqId" .@ ("74ec1d0b" :: String)
              , "tid" .@ ("ThreadId 1" :: String)
              ]
          , loggedMessageMessage = "Logged from 'monad-logger'"
          }
    }