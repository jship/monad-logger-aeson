{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module TestCase.LogOtherNS.MetadataYesThreadContextNo
  ( testCase
  ) where

import Control.Monad.Logger.Aeson ((.=), Loc(..), LogLevel(..), LoggedMessage(..), Message(..), logOtherNS)
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Time (UTCTime(..))
import TestCase (TestCase(..))
import qualified Control.Monad.Logger.Aeson.Internal as Internal
import qualified Data.Time as Time

testCase :: FilePath -> TestCase
testCase logFilePath =
  TestCase
    { actionUnderTest = do
        logOtherNS "tests" (LevelOther "foo") $ "With metadata" :#
          [ "a" .= (42 :: Int)
          , "b" .= ("x" :: String)
          ]
    , logFilePath
    , expectedValue =
        [aesonQQ|
          {
            "time": "2022-05-07T20:03:54.0000000Z",
            "level": "foo",
            "location": {
              "package": "main",
              "module": "TestCase.LogOtherNS.MetadataYesThreadContextNo",
              "file": "test-suite/TestCase/LogOtherNS/MetadataYesThreadContextNo.hs",
              "line": 19,
              "char": 9
            },
            "source": "tests",
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
          , loggedMessageLevel = LevelOther "foo"
          , loggedMessageLoc =
              Just Loc
                { loc_package = "main"
                , loc_module = "TestCase.LogOtherNS.MetadataYesThreadContextNo"
                , loc_filename = "test-suite/TestCase/LogOtherNS/MetadataYesThreadContextNo.hs"
                , loc_start = (19, 9)
                , loc_end = (0, 0)
                }
          , loggedMessageLogSource = Just "tests"
          , loggedMessageThreadContext = Internal.emptyKeyMap
          , loggedMessageText = "With metadata"
          , loggedMessageMeta =
              Internal.keyMapFromList
                [ "a" .= (42 :: Int)
                , "b" .= ("x" :: String)
                ]
          }
    }
