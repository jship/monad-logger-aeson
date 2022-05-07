{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.Monad.Logger.CallStack.JSON
  ( Message(..)
  , LoggedMessage(..)

  , logDebug
  , logInfo
  , logWarn
  , logError
  , logOther
  , logDebugCS
  , logInfoCS
  , logWarnCS
  , logErrorCS
  , logOtherCS
  , logDebugNS
  , logInfoNS
  , logWarnNS
  , logErrorNS
  , logOtherNS

  , withThreadContext

  , runFileLoggingT
  , runHandleLoggingT
  , runStdoutLoggingT
  , runStderrLoggingT
  , runFastLoggingT

  , defaultOutput
  , handleOutput
  , fastLoggerOutput
  , defaultLogStr

  , defaultHandleFromLevel

  , module Log
  ) where

-- N.B. This import is not grouped with the others as this makes it easier to
-- cross-reference with this module's exports.
import Control.Monad.Logger as Log hiding
  ( logDebug
  , logInfo
  , logWarn
  , logError
  , logOther
  , logDebugCS
  , logInfoCS
  , logWarnCS
  , logErrorCS
  , logOtherCS
  , logWithoutLoc -- No re-export, as the 'log*NS' here use call stack for loc
  , logDebugNS
  , logInfoNS
  , logWarnNS
  , logErrorNS
  , logOtherNS
  , runFileLoggingT
  , runStderrLoggingT
  , runStdoutLoggingT
  , defaultOutput
  , defaultLogStr
  )

import Control.Exception.Lifted (bracket)
import Control.Monad.Base (MonadBase(liftBase))
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger.CallStack.JSON.Internal (LoggedMessage(..), Message(..))
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Data.Aeson.Types (Pair)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Stack (CallStack, HasCallStack, callStack)
import System.IO
  ( BufferMode(LineBuffering), IOMode(AppendMode), Handle, hClose, hSetBuffering, openFile, stderr
  , stdout
  )
import System.Log.FastLogger (LoggerSet)
import qualified Context
import qualified Control.Monad.Logger.CallStack.JSON.Internal as Internal
import qualified Data.ByteString.Char8 as BS8
import qualified Data.HashMap.Strict as HashMap
import qualified System.Log.FastLogger as FastLogger

-- | Logs a message with the location provided by an implicit 'CallStack'.
--
-- @since 0.1.0.0
logDebug :: (HasCallStack, MonadLogger m) => Message -> m ()
logDebug = logDebugCS callStack

-- | See 'logDebug'
--
-- @since 0.1.0.0
logInfo :: (HasCallStack, MonadLogger m) => Message -> m ()
logInfo = logInfoCS callStack

-- | See 'logDebug'
--
-- @since 0.1.0.0
logWarn :: (HasCallStack, MonadLogger m) => Message -> m ()
logWarn = logWarnCS callStack

-- | See 'logDebug'
--
-- @since 0.1.0.0
logError :: (HasCallStack, MonadLogger m) => Message -> m ()
logError = logErrorCS callStack

-- | See 'logDebug'
--
-- @since 0.1.0.0
logOther :: (HasCallStack, MonadLogger m) => LogLevel -> Message -> m ()
logOther = logOtherCS callStack

-- | Logs a message with location given by 'CallStack'.
--
-- @since 0.1.0.0
logDebugCS :: (MonadLogger m) => CallStack -> Message -> m ()
logDebugCS cs msg = Internal.logCS cs "" LevelDebug msg

-- | See 'logDebugCS'
--
-- @since 0.1.0.0
logInfoCS :: (MonadLogger m) => CallStack -> Message -> m ()
logInfoCS cs msg = Internal.logCS cs "" LevelInfo msg

-- | See 'logDebugCS'
--
-- @since 0.1.0.0
logWarnCS :: (MonadLogger m) => CallStack -> Message -> m ()
logWarnCS cs msg = Internal.logCS cs "" LevelWarn msg

-- | See 'logDebugCS'
--
-- @since 0.1.0.0
logOtherCS :: (MonadLogger m) => CallStack -> LogLevel -> Message -> m ()
logOtherCS cs lvl msg = Internal.logCS cs "" lvl msg

-- | See 'logDebugCS'
--
-- @since 0.1.0.0
logErrorCS :: (MonadLogger m) => CallStack -> Message -> m ()
logErrorCS cs msg = Internal.logCS cs "" LevelError msg

-- | Note that the @monad-logger@ version does not log location info. This
-- @monad-logger-json@ version logs location info via call stack.
logDebugNS :: (HasCallStack, MonadLogger m) => LogSource -> Message -> m ()
logDebugNS src = Internal.logCS callStack src LevelDebug

-- | Note that the @monad-logger@ version does not log location info. This
-- @monad-logger-json@ version logs location info via call stack.
logInfoNS :: (HasCallStack, MonadLogger m) => LogSource -> Message -> m ()
logInfoNS src = Internal.logCS callStack src LevelInfo

-- | Note that the @monad-logger@ version does not log location info. This
-- @monad-logger-json@ version logs location info via call stack.
logWarnNS :: (HasCallStack, MonadLogger m) => LogSource -> Message -> m ()
logWarnNS src = Internal.logCS callStack src LevelWarn

-- | Note that the @monad-logger@ version does not log location info. This
-- @monad-logger-json@ version logs location info via call stack.
logErrorNS :: (HasCallStack, MonadLogger m) => LogSource -> Message -> m ()
logErrorNS src = Internal.logCS callStack src LevelError

-- | Note that the @monad-logger@ version does not log location info. This
-- @monad-logger-json@ version logs location info via call stack.
logOtherNS :: (HasCallStack, MonadLogger m) => LogSource -> LogLevel -> Message -> m ()
logOtherNS = Internal.logCS callStack

-- | Stub. Add some notes about similarity to MDC (maped diagnostic context
-- from log4j) and ThreadContext (from log4j2).
withThreadContext :: (MonadIO m, MonadMask m) => [Pair] -> m a -> m a
withThreadContext pairs =
  Context.adjust Internal.messageMetaStore \pairsMap ->
    HashMap.union (HashMap.fromList pairs) pairsMap

-- | Run a block using a @MonadLogger@ instance which appends to the specified
-- file.
--
-- @since 0.1.0.0
runFileLoggingT :: (MonadBaseControl IO m) => FilePath -> LoggingT m a -> m a
runFileLoggingT filePath action =
  bracket (liftBase $ openFile filePath AppendMode) (liftBase . hClose) \h -> do
    liftBase $ hSetBuffering h LineBuffering
    runLoggingT action $ defaultOutput h

-- | Run a block using a @MonadLogger@ instance which prints to stderr.
--
-- @since 0.1.0.0
runStderrLoggingT :: LoggingT m a -> m a
runStderrLoggingT = flip runLoggingT (defaultOutput stderr)

-- | Run a block using a @MonadLogger@ instance which prints to stdout.
--
-- @since 0.1.0.0
runStdoutLoggingT :: LoggingT m a -> m a
runStdoutLoggingT = flip runLoggingT (defaultOutput stdout)

-- | Run a block using a @MonadLogger@ instance which prints to a 'Handle'
-- determined by the log message's 'LogLevel'. A common use case for this
-- function is to log warn/error messages to @stderr@ and debug/info messages
-- to @stdout@.
--
-- @since 0.1.0.0
runHandleLoggingT :: (LogLevel -> Handle) -> LoggingT m a -> m a
runHandleLoggingT = flip runLoggingT . handleOutput

-- | Run a block using a @MonadLogger@ instance which appends to the specified
-- 'LoggerSet'.
--
-- @since 0.1.0.0
runFastLoggingT :: LoggerSet -> LoggingT m a -> m a
runFastLoggingT loggerSet = flip runLoggingT (fastLoggerOutput loggerSet)

defaultOutput
  :: Handle
  -> Loc
  -> LogSource
  -> LogLevel
  -> LogStr
  -> IO ()
defaultOutput handle = handleOutput (const handle)

handleOutput
  :: (LogLevel -> Handle)
  -> Loc
  -> LogSource
  -> LogLevel
  -> LogStr
  -> IO ()
handleOutput levelToHandle =
  Internal.defaultOutputWith \logLevel bytes -> do
    BS8.hPutStrLn (levelToHandle logLevel) bytes

fastLoggerOutput
  :: LoggerSet
  -> Loc
  -> LogSource
  -> LogLevel
  -> LogStr
  -> IO ()
fastLoggerOutput loggerSet =
  Internal.defaultOutputWith \_logLevel bytes -> do
    FastLogger.pushLogStrLn loggerSet $ toLogStr bytes

defaultLogStr
  :: UTCTime
  -> [Pair]
  -> Loc
  -> LogSource
  -> LogLevel
  -> LogStr
  -> LogStr
defaultLogStr now threadContext loc logSource logLevel logStr =
  toLogStr
    $ Internal.defaultLogStrBS now threadContext loc logSource logLevel logStr

defaultHandleFromLevel :: (Text -> Handle) -> LogLevel -> Handle
defaultHandleFromLevel otherLevelToHandle = \case
  LevelDebug -> stdout
  LevelInfo -> stdout
  LevelWarn -> stderr
  LevelError -> stderr
  LevelOther otherLevel -> otherLevelToHandle otherLevel
