{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module TestCase.MonadLogger.LogDebugN.ThreadContextNo
  ( testCase
  ) where

import Control.Monad.Logger.Aeson ((.@), LogLevel(..), LoggedMessage(..))
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Time (UTCTime(..))
import TestCase (TestCase(..))
import qualified Control.Monad.Logger.CallStack as ML
import qualified Data.Time as Time

testCase :: FilePath -> TestCase
testCase logFilePath =
  TestCase
    { actionUnderTest = do
        ML.logDebugN "Logged from 'monad-logger'"
    , logFilePath
    , expectedValue =
        [aesonQQ|
          {
            "time": "2022-05-07T20:03:54.0000000Z",
            "level": "debug",
            "context": {
              "tid": "ThreadId 1"
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
          , loggedMessageLoc = Nothing
          , loggedMessageLogSource = Nothing
          , loggedMessageThreadContext = ["tid" .@ ("ThreadId 1" :: String)]
          , loggedMessageMessage = "Logged from 'monad-logger'"
          }
    }
