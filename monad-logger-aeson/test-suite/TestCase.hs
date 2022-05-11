{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
module TestCase
  ( TestCase(..)
  , runTest
  , withTempLogFile
  ) where

import Control.Monad.Logger.CallStack.JSON
  ( LoggedMessage(..), Message(..), LoggingT, runFileLoggingT
  )
import Data.Aeson (Result(..), Value)
import Data.Aeson.Diff (Patch(..))
import GHC.Stack (HasCallStack)
import System.IO (IOMode(..))
import Test.Hspec (Expectation, expectationFailure, shouldBe)
import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Diff as Diff
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List as List
import qualified System.Directory as Directory
import qualified System.IO as IO

data TestCase = TestCase
  { actionUnderTest :: LoggingT IO ()
  , logFilePath :: FilePath
  , expectedValue :: Value
  , expectedPatch :: Value
  , expectedLoggedMessage :: LoggedMessage
  }

runTest :: (HasCallStack) => TestCase -> IO ()
runTest testCase = do
  clearFile logFilePath
  runFileLoggingT logFilePath actionUnderTest
  curLine <- IO.withFile logFilePath ReadMode BS8.hGetLine
  case Aeson.decodeStrict' @Value curLine of
    Nothing -> expectationFailure $ "Failed to parse log line as Value: " <> show curLine
    Just actualValue -> do
      case Aeson.fromJSON expectedPatch of
        Error errStr -> do
          expectationFailure $ "Patch is invalid: " <> show errStr
        Success expectedPatch' -> do
          actualValue `shouldMatchWithPatch` (expectedValue, expectedPatch')
          Diff.patch expectedPatch' actualValue `shouldBe` Success expectedValue
          case Aeson.fromJSON expectedValue of
            Error errStr -> do
              expectationFailure $ "LoggedMessage parse failed: " <> errStr
            Success actualLoggedMessage -> do
              actualLoggedMessage `shouldMatchLoggedMessage` expectedLoggedMessage
  where
  TestCase
    { actionUnderTest
    , logFilePath
    , expectedValue
    , expectedPatch
    , expectedLoggedMessage
    } = testCase

withTempLogFile :: (FilePath -> IO ()) -> IO ()
withTempLogFile f = do
  Exception.bracket
    (IO.openTempFile "." "monad-logger-aeson")
    (\(filePath, handle) -> IO.hClose handle *> Directory.removeFile filePath)
    \(filePath, handle) -> do
      IO.hClose handle
      f filePath

shouldMatchWithPatch :: (HasCallStack) => Value -> (Value, Patch) -> Expectation
shouldMatchWithPatch actualValue (expectedValue, expectedPatch) = do
  Diff.diff actualValue expectedValue `shouldBe` expectedPatch

shouldMatchLoggedMessage
  :: (HasCallStack)
  => LoggedMessage
  -> LoggedMessage
  -> Expectation
shouldMatchLoggedMessage x y =
  sortPairs x `shouldBe` sortPairs y
  where
  sortPairs lm =
    lm
      { loggedMessageThreadContext = List.sort $ loggedMessageThreadContext lm
      , loggedMessageMessage =
          case loggedMessageMessage lm of
            messageText :# messageMeta -> messageText :# List.sort messageMeta
      }

clearFile :: FilePath -> IO ()
clearFile filePath = IO.withFile filePath WriteMode $ flip IO.hSetFileSize 0
