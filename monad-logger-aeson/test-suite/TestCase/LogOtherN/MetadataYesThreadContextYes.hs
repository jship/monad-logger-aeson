{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module TestCase.LogOtherN.MetadataYesThreadContextYes
  ( testCase
  ) where

import Control.Monad.Logger.Aeson
  ( (.=), Loc(..), LogLevel(..), LoggedMessage(..), Message(..), logOtherN, withThreadContext
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
          logOtherN (LevelOther "foo") $ "With metadata" :#
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
              "module": "TestCase.LogOtherN.MetadataYesThreadContextYes",
              "file": "test-suite/TestCase/LogOtherN/MetadataYesThreadContextYes.hs",
              "line": 22,
              "char": 11
            },
            "context": {
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
                , loc_module = "TestCase.LogOtherN.MetadataYesThreadContextYes"
                , loc_filename = "test-suite/TestCase/LogOtherN/MetadataYesThreadContextYes.hs"
                , loc_start = (22, 11)
                , loc_end = (0, 0)
                }
          , loggedMessageLogSource = Nothing
          , loggedMessageThreadContext =
              Internal.keyMapFromList
                [ "reqId" .= ("74ec1d0b" :: String)
                ]
          , loggedMessageText = "With metadata"
          , loggedMessageMeta =
              Internal.keyMapFromList
                [ "a" .= (42 :: Int)
                , "b" .= ("x" :: String)
                ]
          }
    }
