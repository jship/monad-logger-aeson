{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module TestCase.MonadLogger.LogDebugN.ThreadContextYes
  ( testCase
  ) where

import Control.Monad.Logger.Aeson ((.@), LogLevel(..), LoggedMessage(..), withThreadContext)
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
          ML.logDebugN "Logged from 'monad-logger'"
    , logFilePath
    , expectedValue =
        [aesonQQ|
          {
            "time": "2022-05-07T20:03:54.0000000Z",
            "level": "debug",
            "context": {
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
          , loggedMessageThreadContext =
              [ "reqId" .@ ("74ec1d0b" :: String)
              ]
          , loggedMessageMessage = "Logged from 'monad-logger'"
          }
    }
