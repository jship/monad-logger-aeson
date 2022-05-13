{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module TestCase.LogErrorNS.MetadataYesThreadContextYes
  ( testCase
  ) where

import Control.Monad.Logger.Aeson
  ( (.@), Loc(..), LogLevel(..), LoggedMessage(..), Message(..), logErrorNS, withThreadContext
  )
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Time (UTCTime(..))
import TestCase (TestCase(..))
import qualified Data.Time as Time

testCase :: FilePath -> TestCase
testCase logFilePath =
  TestCase
    { actionUnderTest = do
        withThreadContext ["reqId" .@ ("74ec1d0b" :: String)] do
          logErrorNS "tests" $ "With metadata" :#
            [ "a" .@ (42 :: Int)
            , "b" .@ ("x" :: String)
            ]
    , logFilePath
    , expectedValue =
        [aesonQQ|
          {
            "time": "2022-05-07T20:03:54.0000000Z",
            "level": "error",
            "location": {
              "package": "main",
              "module": "TestCase.LogErrorNS.MetadataYesThreadContextYes",
              "file": "test-suite/TestCase/LogErrorNS/MetadataYesThreadContextYes.hs",
              "line": 22,
              "char": 11
            },
            "source": "tests",
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
          , loggedMessageLevel = LevelError
          , loggedMessageLoc =
              Just Loc
                { loc_package = "main"
                , loc_module = "TestCase.LogErrorNS.MetadataYesThreadContextYes"
                , loc_filename = "test-suite/TestCase/LogErrorNS/MetadataYesThreadContextYes.hs"
                , loc_start = (22, 11)
                , loc_end = (0, 0)
                }
          , loggedMessageLogSource = Just "tests"
          , loggedMessageThreadContext =
              [ "reqId" .@ ("74ec1d0b" :: String)
              , "tid" .@ ("ThreadId 1" :: String)
              ]
          , loggedMessageMessage = "With metadata" :#
              [ "a" .@ (42 :: Int)
              , "b" .@ ("x" :: String)
              ]
          }
    }
