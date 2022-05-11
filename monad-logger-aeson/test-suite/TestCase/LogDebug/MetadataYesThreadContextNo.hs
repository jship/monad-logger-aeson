{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module TestCase.LogDebug.MetadataYesThreadContextNo
  ( testCase
  ) where

import Control.Monad.Logger.CallStack.JSON
  ( Loc(..), LogLevel(..), LoggedMessage(..), Message(..), logDebug
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
        logDebug $ "With metadata" :#
          [ "a" .= (42 :: Int)
          , "b" .= ("x" :: String)
          ]
    , logFilePath
    , expectedValue =
        [aesonQQ|
          {
            "time": "2022-05-07T20:03:54.0000000Z",
            "level": "debug",
            "location": {
              "package": "main",
              "module": "TestCase.LogDebug.MetadataYesThreadContextNo",
              "file": "test-suite/TestCase/LogDebug/MetadataYesThreadContextNo.hs",
              "line": 21,
              "char": 9
            },
            "context": {
              "tid": "ThreadId 1"
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
          , loggedMessageLevel = LevelDebug
          , loggedMessageLoc =
              Just Loc
                { loc_package = "main"
                , loc_module = "TestCase.LogDebug.MetadataYesThreadContextNo"
                , loc_filename = "test-suite/TestCase/LogDebug/MetadataYesThreadContextNo.hs"
                , loc_start = (21, 9)
                , loc_end = (0, 0)
                }
          , loggedMessageLogSource = Nothing
          , loggedMessageThreadContext = ["tid" .= ("ThreadId 1" :: String)]
          , loggedMessageMessage = "With metadata" :#
              [ "a" .= (42 :: Int)
              , "b" .= ("x" :: String)
              ]
          }
    }