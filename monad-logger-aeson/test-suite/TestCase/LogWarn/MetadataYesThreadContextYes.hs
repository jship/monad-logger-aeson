{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module TestCase.LogWarn.MetadataYesThreadContextYes
  ( testCase
  ) where

import Control.Monad.Logger.CallStack.JSON
  ( Loc(..), LogLevel(..), LoggedMessage(..), Message(..), logWarn, withThreadContext
  )
import Data.Aeson ((.=))
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Time (UTCTime(..))
import TestCase (TestCase(..))
import qualified Data.Time as Time

testCase :: FilePath -> TestCase
testCase logFilePath =
  TestCase
    { actionUnderTest = do
        withThreadContext ["reqId" .= ("74ec1d0b" :: String)] do
          logWarn $ "With metadata" :#
            [ "a" .= (42 :: Int)
            , "b" .= ("x" :: String)
            ]
    , logFilePath
    , expectedValue =
        [aesonQQ|
          {
            "time": "2022-05-07T20:03:54.0000000Z",
            "level": "warn",
            "location": {
              "package": "main",
              "module": "TestCase.LogWarn.MetadataYesThreadContextYes",
              "file": "test-suite/TestCase/LogWarn/MetadataYesThreadContextYes.hs",
              "line": 23,
              "char": 11
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
          , loggedMessageLevel = LevelWarn
          , loggedMessageLoc =
              Just Loc
                { loc_package = "main"
                , loc_module = "TestCase.LogWarn.MetadataYesThreadContextYes"
                , loc_filename = "test-suite/TestCase/LogWarn/MetadataYesThreadContextYes.hs"
                , loc_start = (23, 11)
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
