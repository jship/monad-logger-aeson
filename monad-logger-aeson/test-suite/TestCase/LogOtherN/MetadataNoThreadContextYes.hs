{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module TestCase.LogOtherN.MetadataNoThreadContextYes
  ( testCase
  ) where

import Control.Monad.Logger.Aeson
  ( (.=), Loc(..), LogLevel(..), LoggedMessage(..), logOtherN, withThreadContext
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
        withThreadContext ["reqId" .= ("74ec1d0b" :: String)] $ do
          logOtherN (LevelOther "foo") "No metadata"
    , logFilePath
    , expectedValue =
        [aesonQQ|
          {
            "time": "2022-05-07T20:03:54.0000000Z",
            "level": "foo",
            "location": {
              "package": "main",
              "module": "TestCase.LogOtherN.MetadataNoThreadContextYes",
              "file": "test-suite/TestCase/LogOtherN/MetadataNoThreadContextYes.hs",
              "line": 22,
              "char": 11
            },
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
          , loggedMessageLevel = LevelOther "foo"
          , loggedMessageLoc =
              Just Loc
                { loc_package = "main"
                , loc_module = "TestCase.LogOtherN.MetadataNoThreadContextYes"
                , loc_filename = "test-suite/TestCase/LogOtherN/MetadataNoThreadContextYes.hs"
                , loc_start = (22, 11)
                , loc_end = (0, 0)
                }
          , loggedMessageLogSource = Nothing
          , loggedMessageThreadContext =
              Internal.keyMapFromList
                [ "reqId" .= ("74ec1d0b" :: String)
                ]
          , loggedMessageText = "No metadata"
          , loggedMessageMeta = Internal.emptyKeyMap
          }
    }
