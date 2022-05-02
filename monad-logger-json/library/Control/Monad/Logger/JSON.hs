{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Control.Monad.Logger.JSON
  ( module Log
  , runFileLoggingT
  , runStdoutLoggingT
  , runStderrLoggingT
  , defaultOutput

  , Message(..)

  , logSomeJSON -- TODO: Remove later!
  ) where

import Control.Exception.Lifted (bracket)
import Control.Monad.Base (MonadBase(liftBase))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger as Log hiding
  ( defaultLogStr, defaultOutput, logDebug, logDebugSH, logError, logErrorSH, logInfo, logInfoSH
  , logOther, logOtherSH, logWarn, logWarnSH, runFileLoggingT, runStderrLoggingT, runStdoutLoggingT
  )
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Data.Aeson (KeyValue((.=)), Value(..), (.:), (.:?), Encoding, FromJSON, ToJSON)
import Data.Aeson.Types (Pair)
import Data.ByteString.Char8 (ByteString)
import Data.String (IsString)
import Data.Text (Text)
import Data.Time (UTCTime)
import System.IO
  ( BufferMode(LineBuffering), IOMode(AppendMode), Handle, hClose, hSetBuffering, openFile, stderr
  , stdout
  )
import System.Log.FastLogger.Internal (LogStr(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.ByteString.Builder as ByteString.Builder
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Encoding.Error as Text.Encoding.Error
import qualified Data.Time as Time

-- TODO: Remove later - just for quick manual testing
-- In reality, we would provide the same variants that 'monad-logger' provides.
logSomeJSON :: (MonadLogger m) => LogSource -> LogLevel -> Message -> m ()
logSomeJSON logSource logLevel message = do
  monadLoggerLog defaultLoc logSource logLevel $ toLogStr message

data LogItem = LogItem
  { logItemTimestamp :: UTCTime
  , logItemLoc :: Loc
  , logItemLogSource :: LogSource
  , logItemLevel :: LogLevel
  , logItemMessage :: Message
  }

data Message = Text :& [Pair]
  deriving Show -- TODO: Remove later!
infixr 5 :&

instance FromJSON Message where
  parseJSON = Aeson.withObject "Message" \obj -> do
    messageText <- obj .: "text"
    messageMeta <- do
      obj .:? "meta" >>= \case
        Just (Object hashMap) -> pure $ Aeson.KeyMap.toList hashMap
        _ -> pure []
    pure $ messageText :& messageMeta

instance ToJSON Message where
  toJSON message =
    Aeson.object $
      "text" .= messageText
        : if null messageMeta then
            []
          else
            ["meta" .= Aeson.object messageMeta]
    where
    messageText :& messageMeta = message

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

    messageText :& messageMeta = message

instance ToLogStr Message where
  toLogStr = toLogStr . Aeson.encode

instance IsString Message where
  fromString string = Text.pack string :& []

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
      Nothing -> mkLogItem $ logStrText :& []
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
runStderrLoggingT :: (MonadIO m) => LoggingT m a -> m a
runStderrLoggingT = (`runLoggingT` defaultOutput stderr)

-- | Run a block using a @MonadLogger@ instance which prints to stdout.
--
-- @since 0.1.0.0
runStdoutLoggingT :: (MonadIO m) => LoggingT m a -> m a
runStdoutLoggingT = (`runLoggingT` defaultOutput stdout)

isDefaultLoc :: Loc -> Bool
isDefaultLoc (Loc "<unknown>" "<unknown>" "<unknown>" (0,0) (0,0)) = True
isDefaultLoc _ = False
