{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.Monad.Logger.Aeson
  ( -- * Intro
    -- $intro

    -- * Types
    Message(..)
  , (.@)
  , LoggedMessage(..)

    -- * Logging functions
    -- ** Implicit call stack, no @LogSource@
  , logDebug
  , logInfo
  , logWarn
  , logError
  , logOther
    -- ** Explicit call stack, no @LogSource@
  , logDebugCS
  , logInfoCS
  , logWarnCS
  , logErrorCS
  , logOtherCS
    -- ** Implicit call stack and @LogSource@
  , logDebugNS
  , logInfoNS
  , logWarnNS
  , logErrorNS
  , logOtherNS

    -- ** Thread context
  , withThreadContext
  , myThreadContext

    -- * @LoggingT@ runners
  , runFileLoggingT
  , runHandleLoggingT
  , runStdoutLoggingT
  , runStderrLoggingT
  , runFastLoggingT

    -- * Utilities for defining our own loggers
  , defaultOutput
  , handleOutput
  , fastLoggerOutput
  , defaultOutputWith
  , defaultOutputOptions
  , OutputOptions
  , outputIncludeThreadId
  , outputBaseThreadContext
  , defaultLogStr
  , defaultHandleFromLevel

    -- * Re-exports from @monad-logger@
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

import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger.Aeson.Internal (LoggedMessage(..), Message(..), OutputOptions(..))
import Data.Aeson (Value(String), (.=), Key, ToJSON)
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
import qualified Control.Concurrent as Concurrent
import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.Logger.Aeson.Internal as Internal
import qualified Data.ByteString.Char8 as BS8
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified System.Log.FastLogger as FastLogger

-- | Synonym for '.=' from @aeson@.
(.@) :: (ToJSON v) => Key -> v -> Pair
(.@) = (.=)

-- | Logs a 'Message' with the location provided by an implicit 'CallStack'.
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

-- | Logs a 'Message' with location given by 'CallStack'.
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

-- | See 'logDebugCS'
--
-- @since 0.1.0.0
logDebugNS :: (HasCallStack, MonadLogger m) => LogSource -> Message -> m ()
logDebugNS src = Internal.logCS callStack src LevelDebug

-- | See 'logDebugNS'
--
-- @since 0.1.0.0
logInfoNS :: (HasCallStack, MonadLogger m) => LogSource -> Message -> m ()
logInfoNS src = Internal.logCS callStack src LevelInfo

-- | See 'logDebugNS'
--
-- @since 0.1.0.0
logWarnNS :: (HasCallStack, MonadLogger m) => LogSource -> Message -> m ()
logWarnNS src = Internal.logCS callStack src LevelWarn

-- | See 'logDebugNS'
--
-- @since 0.1.0.0
logErrorNS :: (HasCallStack, MonadLogger m) => LogSource -> Message -> m ()
logErrorNS src = Internal.logCS callStack src LevelError

-- | See 'logDebugNS'
--
-- @since 0.1.0.0
logOtherNS :: (HasCallStack, MonadLogger m) => LogSource -> LogLevel -> Message -> m ()
logOtherNS = Internal.logCS callStack

-- | This function lets us register structured, contextual info for the duration
-- of the provided action. All messages logged within the provided action will
-- automatically include this contextual info. This function is thread-safe, as
-- the contextual info is scoped to the calling thread only.
--
-- This function is additive: if we nest calls to it, each nested call will add
-- to the existing thread context. In the case of overlapping keys, the nested
-- call's 'Pair' value(s) will win. Whenever the inner action completes, the
-- thread context is rolled back to its value set in the enclosing action.
--
-- If we wish to include the existing thread context from one thread in another
-- thread, we must register the thread context explicitly on that other thread.
-- 'myThreadContext' can be leveraged in this case.
--
-- Registering thread context for messages can be useful in many scenarios. One
-- particularly apt scenario is in @wai@ middlewares. We can generate an ID for
-- each incoming request then include it in the thread context. Now all messages
-- subsequently logged from our endpoint handler will automatically include that
-- request ID:
--
-- > import Control.Monad.Logger.Aeson ((.@), withThreadContext)
-- > import Network.Wai (Middleware)
-- > import qualified Data.UUID.V4 as UUID
-- >
-- > addRequestId :: Middleware
-- > addRequestId app = \request sendResponse -> do
-- >   uuid <- UUID.nextRandom
-- >   withThreadContext ["requestId" .@ uuid] do
-- >     app request sendResponse
--
-- If we're coming from a Java background, it may be helpful for us to draw
-- parallels between this function and @log4j2@'s @ThreadContext@ (or perhaps
-- @log4j@'s @MDC@). They all enable the same thing: setting some thread-local,
-- structured info that will be automatically pulled into each logged message.
--
-- @since 0.1.0.0
withThreadContext :: (MonadIO m, MonadMask m) => [Pair] -> m a -> m a
withThreadContext pairs =
  Context.adjust Internal.threadContextStore \pairsMap ->
    HashMap.union (HashMap.fromList pairs) pairsMap

-- | This function lets us retrieve the calling thread's thread context. For
-- more detail, we can consult the docs for 'withThreadContext'.
--
-- Note that even though the type signature lists 'MonadThrow' as a required
-- constraint, the library guarantees that 'myThreadContext' will never throw.
--
-- @since 0.1.0.0
myThreadContext :: (MonadIO m, MonadThrow m) => m [Pair]
myThreadContext = do
  Context.mines Internal.threadContextStore HashMap.toList

-- | Run a block using a 'MonadLogger' instance which appends to the specified
-- file.
--
-- Note that this differs from the @monad-logger@ version in its constraints.
-- We use the @exceptions@ package's 'MonadMask' here for bracketing, rather
-- than @monad-control@.
--
-- @since 0.1.0.0
runFileLoggingT :: (MonadIO m, MonadMask m) => FilePath -> LoggingT m a -> m a
runFileLoggingT filePath action =
  Catch.bracket (liftIO $ openFile filePath AppendMode) (liftIO . hClose) \h -> do
    liftIO $ hSetBuffering h LineBuffering
    runLoggingT action $ defaultOutput h

-- | Run a block using a 'MonadLogger' instance which prints to 'stderr'.
--
-- @since 0.1.0.0
runStderrLoggingT :: LoggingT m a -> m a
runStderrLoggingT = flip runLoggingT (defaultOutput stderr)

-- | Run a block using a 'MonadLogger' instance which prints to 'stdout'.
--
-- @since 0.1.0.0
runStdoutLoggingT :: LoggingT m a -> m a
runStdoutLoggingT = flip runLoggingT (defaultOutput stdout)

-- | Run a block using a 'MonadLogger' instance which prints to a 'Handle'
-- determined by the log message's 'LogLevel'.
--
-- A common use case for this function is to log warn/error messages to 'stderr'
-- and debug/info messages to 'stdout' in CLIs/tools (see
-- 'defaultHandleFromLevel').
--
-- @since 0.1.0.0
runHandleLoggingT :: (LogLevel -> Handle) -> LoggingT m a -> m a
runHandleLoggingT = flip runLoggingT . handleOutput

-- | Run a block using a 'MonadLogger' instance which appends to the specified
-- 'LoggerSet'.
--
-- @since 0.1.0.0
runFastLoggingT :: LoggerSet -> LoggingT m a -> m a
runFastLoggingT loggerSet = flip runLoggingT (fastLoggerOutput loggerSet)

-- | A default implementation of the action that backs the 'monadLoggerLog'
-- function. It accepts a file handle as the first argument and will log
-- incoming 'LogStr' values wrapped in the JSON structure prescribed by this
-- library.
--
-- This is used in the definition of 'runStdoutLoggingT' and
-- 'runStderrLoggingT':
--
-- @
-- 'runStdoutLoggingT' :: 'LoggingT' m a -> m a
-- 'runStdoutLoggingT' = 'flip' 'runLoggingT' ('defaultOutput' 'stdout')
-- @
--
-- We can instead use 'defaultOutputWith' if we need more control of the output.
--
-- @since 0.1.0.0
defaultOutput
  :: Handle
  -> Loc
  -> LogSource
  -> LogLevel
  -> LogStr
  -> IO ()
defaultOutput handle = handleOutput (const handle)

-- | Given an output action for log messages, this function will produce the
-- default recommended 'OutputOptions'.
--
-- Specific options can be overriden via record update syntax using
-- 'outputIncludeThreadId', 'outputBaseThreadContext', and friends.
--
-- @since 0.1.0.0
defaultOutputOptions :: (LogLevel -> BS8.ByteString -> IO ()) -> OutputOptions
defaultOutputOptions outputAction =
  OutputOptions
    { outputAction
    , outputIncludeThreadId = True
    , outputBaseThreadContext = []
    }

-- | This function is a lower-level helper for implementing the action that
-- backs the 'monadLoggerLog' function.
--
-- We should generally prefer 'defaultOutput' over this function, but this
-- function is available if we do need more control over our output.
--
-- @since 0.1.0.0
defaultOutputWith
  :: OutputOptions
  -> Loc
  -> LogSource
  -> LogLevel
  -> LogStr
  -> IO ()
defaultOutputWith outputOptions location logSource logLevel msg = do
  now <- Time.getCurrentTime
  threadIdText <- fmap (Text.pack . show) Concurrent.myThreadId
  threadContext <- Context.mines Internal.threadContextStore \hashMap ->
    HashMap.toList
      $ ( if outputIncludeThreadId then
            HashMap.insert "tid" $ String threadIdText
          else
            id
        )
      $ HashMap.union hashMap
      $ baseThreadContextHashMap
  outputAction logLevel
    $ Internal.defaultLogStrBS now threadContext location logSource logLevel msg
  where
  baseThreadContextHashMap = HashMap.fromList outputBaseThreadContext
  OutputOptions
    { outputAction
    , outputIncludeThreadId
    , outputBaseThreadContext
    } = outputOptions

-- | An implementation of the action that backs the 'monadLoggerLog' function,
-- where the 'Handle' destination for each log message is determined by the log
-- message's 'LogLevel'. This function will log incoming 'LogStr' values wrapped
-- in the JSON structure prescribed by this library.
--
-- This is used in the definition of 'runHandleLoggingT':
--
-- @
-- 'runHandleLoggingT' :: ('LogLevel' -> 'Handle') -> 'LoggingT' m a -> m a
-- 'runHandleLoggingT' = 'flip' 'runLoggingT' . 'handleOutput'
-- @
--
-- @since 0.1.0.0
handleOutput
  :: (LogLevel -> Handle)
  -> Loc
  -> LogSource
  -> LogLevel
  -> LogStr
  -> IO ()
handleOutput levelToHandle =
  defaultOutputWith $ defaultOutputOptions \logLevel bytes -> do
    BS8.hPutStrLn (levelToHandle logLevel) bytes

-- | An implementation of the action that backs the 'monadLoggerLog' function,
-- where log messages are written to a provided 'LoggerSet'. This function will
-- log incoming 'LogStr' values wrapped in the JSON structure prescribed by this
-- library.
--
-- This is used in the definition of 'runFastLoggingT':
--
-- @
-- 'runFastLoggingT' :: 'LoggerSet' -> 'LoggingT' m a -> m a
-- 'runFastLoggingT' loggerSet = 'flip' 'runLoggingT' ('fastLoggerOutput' loggerSet)
-- @
--
-- @since 0.1.0.0
fastLoggerOutput
  :: LoggerSet
  -> Loc
  -> LogSource
  -> LogLevel
  -> LogStr
  -> IO ()
fastLoggerOutput loggerSet =
  defaultOutputWith $ defaultOutputOptions \_logLevel bytes -> do
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

-- | This function maps the possible 'LogLevel' values to 'Handle' values.
-- Specifically, 'LevelDebug' and 'LevelInfo' map to 'stdout', while 'LevelWarn'
-- and 'LevelError' map to 'stderr'. The function is most useful for CLIs/tools
-- (see 'runHandleLoggingT').
--
-- The input function discriminating 'Text' is used to determine the 'Handle'
-- mapping for 'LevelOther'. For example, if we wish for all 'LevelOther'
-- messages to be logged to 'stderr', we can supply @(const stderr)@ as the
-- value for this input function.
--
-- @since 0.1.0.0
defaultHandleFromLevel :: (Text -> Handle) -> LogLevel -> Handle
defaultHandleFromLevel otherLevelToHandle = \case
  LevelDebug -> stdout
  LevelInfo -> stdout
  LevelWarn -> stderr
  LevelError -> stderr
  LevelOther otherLevel -> otherLevelToHandle otherLevel

-- $intro
--
-- This module...
