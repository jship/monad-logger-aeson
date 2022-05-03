{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Control.Monad.Logger.CallStack.JSON
  ( Message(..)

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

  , runFileLoggingT
  , runStdoutLoggingT
  , runStderrLoggingT
  , defaultOutput

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
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Data.Aeson (KeyValue((.=)), Value(..), (.:), (.:?), Encoding, FromJSON, ToJSON)
import Data.Aeson.Types (Pair)
import Data.ByteString.Char8 (ByteString)
import Data.String (IsString)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Stack (SrcLoc(..), CallStack, HasCallStack, callStack, getCallStack)
import System.IO
  ( BufferMode(LineBuffering), IOMode(AppendMode), Handle, hClose, hSetBuffering, openFile, stderr
  , stdout
  )
import System.Log.FastLogger.Internal (LogStr(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.ByteString.Builder as ByteString.Builder
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Encoding.Error as Text.Encoding.Error
import qualified Data.Time as Time

#if MIN_VERSION_aeson(2, 0, 0)
import qualified Data.Aeson.KeyMap as AesonCompat (toList)
#else
import qualified Data.HashMap.Strict as AesonCompat (toList)
#endif

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
logDebugCS :: MonadLogger m => CallStack -> Message -> m ()
logDebugCS cs msg = logCS cs "" LevelDebug msg

-- | See 'logDebugCS'
--
-- @since 0.1.0.0
logInfoCS :: MonadLogger m => CallStack -> Message -> m ()
logInfoCS cs msg = logCS cs "" LevelInfo msg

-- | See 'logDebugCS'
--
-- @since 0.1.0.0
logWarnCS :: MonadLogger m => CallStack -> Message -> m ()
logWarnCS cs msg = logCS cs "" LevelWarn msg

-- | See 'logDebugCS'
--
-- @since 0.1.0.0
logOtherCS :: MonadLogger m => CallStack -> LogLevel -> Message -> m ()
logOtherCS cs lvl msg = logCS cs "" lvl msg

-- | See 'logDebugCS'
--
-- @since 0.1.0.0
logErrorCS :: MonadLogger m => CallStack -> Message -> m ()
logErrorCS cs msg = logCS cs "" LevelError msg

-- | Note that the @monad-logger@ version does not log location info. This
-- @monad-logger-json@ version logs location info via call stack.
logDebugNS :: (HasCallStack, MonadLogger m) => LogSource -> Message -> m ()
logDebugNS src = logCS callStack src LevelDebug

-- | Note that the @monad-logger@ version does not log location info. This
-- @monad-logger-json@ version logs location info via call stack.
logInfoNS :: (HasCallStack, MonadLogger m) => LogSource -> Message -> m ()
logInfoNS src = logCS callStack src LevelInfo

-- | Note that the @monad-logger@ version does not log location info. This
-- @monad-logger-json@ version logs location info via call stack.
logWarnNS :: (HasCallStack, MonadLogger m) => LogSource -> Message -> m ()
logWarnNS src = logCS callStack src LevelWarn

-- | Note that the @monad-logger@ version does not log location info. This
-- @monad-logger-json@ version logs location info via call stack.
logErrorNS :: (HasCallStack, MonadLogger m) => LogSource -> Message -> m ()
logErrorNS src = logCS callStack src LevelError

-- | Note that the @monad-logger@ version does not log location info. This
-- @monad-logger-json@ version logs location info via call stack.
logOtherNS :: (HasCallStack, MonadLogger m) => LogSource -> LogLevel -> Message -> m ()
logOtherNS = logCS callStack

-- | Not exported from 'monad-logger', so copied here.
logCS :: (MonadLogger m)
      => CallStack
      -> LogSource
      -> LogLevel
      -> Message
      -> m ()
logCS cs src lvl msg =
  monadLoggerLog (locFromCS cs) src lvl msg

-- | Not exported from 'monad-logger', so copied here.
mkLoggerLoc :: SrcLoc -> Loc
mkLoggerLoc loc =
  Loc { loc_filename = srcLocFile loc
      , loc_package  = srcLocPackage loc
      , loc_module   = srcLocModule loc
      , loc_start    = ( srcLocStartLine loc
                       , srcLocStartCol loc)
      , loc_end      = ( srcLocEndLine loc
                       , srcLocEndCol loc)
      }

-- | Not exported from 'monad-logger', so copied here.
locFromCS :: CallStack -> Loc
locFromCS cs = case getCallStack cs of
                 ((_, loc):_) -> mkLoggerLoc loc
                 _            -> defaultLoc

data LogItem = LogItem
  { logItemTimestamp :: UTCTime
  , logItemLoc :: Loc
  , logItemLogSource :: LogSource
  , logItemLevel :: LogLevel
  , logItemMessage :: Message
  }

-- | A 'Message' captures a textual component and a metadata component. The
-- metadata component is a list of 'Pair' to support tacking on arbitrary
-- structured data to a log message.
--
-- With the @OverloadedStrings@ extension enabled, 'Message' values can be
-- constructed without metadata fairly conveniently, just as if we were using
-- 'Text' directly:
--
-- > logDebug "Some log message without metadata"
--
-- Metadata may be included in a 'Message' via the ':#' constructor:
--
-- > logDebug $ "Some log message with metadata" :#
-- >   [ "bloorp" .= (42 :: Int)
-- >   , "bonk" .= ("abc" :: Text)
-- >   ]
--
-- The mneomic for the ':#' constructor is that the @#@ symbol is sometimes
-- referred to as a hash, a JSON object can be thought of as a hash map, and
-- so with @:#@ (and enough squinting), we are @cons@-ing a textual message onto
-- a JSON object. Yes, this mnemonic isn't well-typed, but hopefully it still
-- helps!
data Message = Text :# [Pair]

infixr 5 :#

instance FromJSON Message where
  parseJSON = Aeson.withObject "Message" \obj -> do
    messageText <- obj .: "text"
    messageMeta <- do
      obj .:? "meta" >>= \case
        Just (Object hashMap) -> pure $ AesonCompat.toList hashMap
        _ -> pure []
    pure $ messageText :# messageMeta

instance ToJSON Message where
  toJSON message =
    Aeson.object $
      "text" .= messageText
        : if null messageMeta then
            []
          else
            ["meta" .= Aeson.object messageMeta]
    where
    messageText :# messageMeta = message

  toEncoding message =
    Aeson.pairs $
      "text" .= messageText
        <> ( if null messageMeta then
               mempty
             else
               Aeson.pairStr "meta" $ messageMetaEncoding messageMeta
           )
    where
    messageMetaEncoding :: [Pair] -> Encoding
    messageMetaEncoding pairs =
      Aeson.pairs
        $ mconcat
        $ fmap (uncurry (.=)) pairs

    messageText :# messageMeta = message

instance ToLogStr Message where
  toLogStr = toLogStr . Aeson.encode

instance IsString Message where
  fromString string = Text.pack string :# []

logItemEncoding :: LogItem -> Encoding
logItemEncoding logItem =
  Aeson.pairs $
    (Aeson.pairStr "timestamp" $ Aeson.toEncoding logItemTimestamp)
      <> (Aeson.pairStr "level" $ levelEncoding logItemLevel)
      <> ( if isDefaultLoc logItemLoc then
             mempty
           else
             Aeson.pairStr "location" $ locEncoding logItemLoc
         )
      <> ( if Text.null logItemLogSource then
             mempty
           else
             Aeson.pairStr "source" $ Aeson.toEncoding logItemLogSource
         )
      <> (Aeson.pairStr "message" $ Aeson.toEncoding logItemMessage)
  where
  LogItem
    { logItemTimestamp
    , logItemLoc
    , logItemLogSource
    , logItemLevel
    , logItemMessage
    } = logItem

levelEncoding :: LogLevel -> Encoding
levelEncoding = Aeson.text . \case
  LevelDebug -> "debug"
  LevelInfo -> "info"
  LevelWarn -> "warn"
  LevelError -> "error"
  LevelOther t -> t

locEncoding :: Loc -> Encoding
locEncoding loc =
  Aeson.pairs $
    (Aeson.pairStr "package" $ Aeson.string loc_package)
      <> (Aeson.pairStr "module" $ Aeson.string loc_module)
      <> (Aeson.pairStr "file" $ Aeson.string loc_filename)
      <> (Aeson.pairStr "line" $ Aeson.int $ fst loc_start)
      <> (Aeson.pairStr "char" $ Aeson.int $ snd loc_start)
  where
  Loc { loc_filename, loc_package, loc_module, loc_start } = loc

defaultOutput
  :: Handle
  -> Loc
  -> LogSource
  -> LogLevel
  -> LogStr
  -> IO ()
defaultOutput h loc src level msg = do
  now <- Time.getCurrentTime
  ByteString.Char8.hPutStrLn h $ defaultLogStrBS now loc src level msg

defaultLogStrBS
  :: UTCTime
  -> Loc
  -> LogSource
  -> LogLevel
  -> LogStr
  -> ByteString
defaultLogStrBS now loc logSource logLevel logStr =
  ByteString.Lazy.toStrict
    $ Aeson.encodingToLazyByteString
    $ logItemEncoding logItem
  where
  logItem :: LogItem
  logItem =
    case Aeson.decode @Message logStrLBS of
      Nothing -> mkLogItem $ logStrText :# []
      Just message -> mkLogItem message

  mkLogItem :: Message -> LogItem
  mkLogItem message =
    LogItem
      { logItemTimestamp = now
      , logItemLoc = loc
      , logItemLogSource = logSource
      , logItemLevel = logLevel
      , logItemMessage = message
      }

  logStrText :: Text
  logStrText =
    Text.Encoding.decodeUtf8With Text.Encoding.Error.lenientDecode
      $ ByteString.Lazy.toStrict logStrLBS

  logStrLBS = ByteString.Builder.toLazyByteString logStrBuilder
  LogStr _ logStrBuilder = logStr

-- | Run a block using a @MonadLogger@ instance which appends to the specified
-- file.
--
-- @since 0.1.0.0
runFileLoggingT :: (MonadBaseControl IO m) => FilePath -> LoggingT m a -> m a
runFileLoggingT fp logt =
  bracket (liftBase $ openFile fp AppendMode) (liftBase . hClose) \h -> do
    liftBase $ hSetBuffering h LineBuffering
    runLoggingT logt $ defaultOutput h

-- | Run a block using a @MonadLogger@ instance which prints to stderr.
--
-- @since 0.1.0.0
runStderrLoggingT :: LoggingT m a -> m a
runStderrLoggingT = (`runLoggingT` defaultOutput stderr)

-- | Run a block using a @MonadLogger@ instance which prints to stdout.
--
-- @since 0.1.0.0
runStdoutLoggingT :: LoggingT m a -> m a
runStdoutLoggingT = (`runLoggingT` defaultOutput stdout)

isDefaultLoc :: Loc -> Bool
isDefaultLoc (Loc "<unknown>" "<unknown>" "<unknown>" (0,0) (0,0)) = True
isDefaultLoc _ = False
