{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module TestCase.LogErrorN.MetadataYesThreadContextNo
  ( testCase
  ) where

import Control.Monad.Logger.Aeson ((.=), Loc(..), LogLevel(..), LoggedMessage(..), Message(..), logErrorN)
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Time (UTCTime(..))
import TestCase (TestCase(..))
import qualified Control.Monad.Logger.Aeson.Internal as Internal
import qualified Data.Time as Time

testCase :: FilePath -> TestCase
testCase logFilePath =
  TestCase
    { actionUnderTest = do
        logErrorN $ "With metadata" :#
          [ "a" .= (42 :: Int)
          , "b" .= ("x" :: String)
          ]
    , logFilePath
    , expectedValue =
        [aesonQQ|
          {
            "time": "2022-05-07T20:03:54.0000000Z",
            "level": "error",
            "location": {
              "package": "main",
              "module": "TestCase.LogErrorN.MetadataYesThreadContextNo",
              "file": "test-suite/TestCase/LogErrorN/MetadataYesThreadContextNo.hs",
              "line": 19,
              "char": 9
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
                , loc_module = "TestCase.LogErrorN.MetadataYesThreadContextNo"
                , loc_filename = "test-suite/TestCase/LogErrorN/MetadataYesThreadContextNo.hs"
                , loc_start = (19, 9)
                , loc_end = (0, 0)
                }
          , loggedMessageLogSource = Nothing
          , loggedMessageThreadContext = Internal.emptyKeyMap
          , loggedMessageText = "With metadata"
          , loggedMessageMeta =
              Internal.keyMapFromList
                [ "a" .= (42 :: Int)
                , "b" .= ("x" :: String)
                ]
          }
    }
