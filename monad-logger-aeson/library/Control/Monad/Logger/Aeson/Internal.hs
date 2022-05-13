{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
module Control.Monad.Logger.Aeson.Internal
  ( -- * Disclaimer
    -- $disclaimer

    -- ** @Message@-related
    Message(..)
  , (.@)
  , LoggedMessage(..)
  , threadContextStore
  , logCS
  , OutputOptions(..)
  , defaultLogStrBS
  , defaultLogStrLBS
  , messageToLogStr
  , messageEncoding
  , messageSeries

    -- ** @LogItem@-related
  , LogItem(..)
  , logItemEncoding

    -- ** Encoding-related
  , xonCharLogStr
  , xonChar
  , pairsEncoding
  , pairsSeries
  , levelEncoding
  , locEncoding

    -- ** @monad-logger@ internals
  , mkLoggerLoc
  , locFromCS
  , isDefaultLoc
  ) where

import Context (Store)
import Control.Applicative (Applicative(liftA2))
import Control.Monad.Logger (Loc(..), LogLevel(..), MonadLogger(..), ToLogStr(..), LogSource)
import Data.Aeson (KeyValue((.=)), (.:), (.:?), Encoding, FromJSON, ToJSON, Value)
import Data.Aeson.Encoding.Internal (Series(..))
import Data.Aeson.Types (Pair, Parser)
import Data.HashMap.Strict (HashMap)
import Data.String (IsString)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import GHC.Stack (SrcLoc(..), CallStack, getCallStack)
import System.Log.FastLogger.Internal (LogStr(..))
import qualified Context
import qualified Control.Monad.Logger as Logger
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Maybe as Maybe
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Encoding.Error as Text.Encoding.Error
import qualified System.IO.Unsafe as IO.Unsafe

#if MIN_VERSION_aeson(2, 0, 0)
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as AesonCompat (toList)
#else
import qualified Data.HashMap.Strict as AesonCompat (toList)
type Key = Text
#endif

-- | Synonym for '.=' from @aeson@.
(.@) :: (KeyValue kv, ToJSON v) => Key -> v -> kv
(.@) = (.=)

data LoggedMessage = LoggedMessage
  { loggedMessageTimestamp :: UTCTime
  , loggedMessageLevel :: LogLevel
  , loggedMessageLoc :: Maybe Loc -- TODO: Add some tests using @monad-logger@ stuff to check this
  , loggedMessageLogSource :: Maybe LogSource
  , loggedMessageThreadContext :: [Pair]
  , loggedMessageMessage :: Message
  } deriving stock (Eq, Generic, Ord, Show)

instance FromJSON LoggedMessage where
  parseJSON = Aeson.withObject "LoggedMessage" \obj ->
    LoggedMessage
      <$> obj .: "time"
      <*> fmap logLevelFromText (obj .: "level")
      <*> (obj .:? "location" >>= parseLoc)
      <*> obj .:? "source"
      <*> (obj .:? "context" >>= parsePairs)
      <*> (obj .: "message" >>= parseMessage)
    where
    logLevelFromText :: Text -> LogLevel
    logLevelFromText = \case
      "debug" -> LevelDebug
      "info" -> LevelInfo
      "warn" -> LevelWarn
      "error" -> LevelError
      other -> LevelOther other

    parseLoc :: Maybe Value -> Parser (Maybe Loc)
    parseLoc =
      traverse $ Aeson.withObject "Loc" \obj ->
        Loc
          <$> obj .: "file"
          <*> obj .: "package"
          <*> obj .: "module"
          <*> (liftA2 (,) (obj .: "line") (obj .: "char"))
          <*> pure (0, 0)

    parsePairs :: Maybe Value -> Parser [Pair]
    parsePairs = \case
      Nothing -> pure []
      Just value -> flip (Aeson.withObject "[Pair]") value \obj -> do
        pure $ AesonCompat.toList obj

    parseMessage :: Value -> Parser Message
    parseMessage = Aeson.withObject "Message" \obj ->
      (:#)
        <$> obj .: "text"
        <*> (obj .:? "meta" >>= parsePairs)

instance ToJSON LoggedMessage where
  toJSON loggedMessage =
    Aeson.object $ Maybe.catMaybes
      [ Just $ "time" .@ loggedMessageTimestamp
      , Just $ "level" .@ logLevelToText loggedMessageLevel
      , case loggedMessageLoc of
          Nothing -> Nothing
          Just loc -> Just $ "location" .@ locToJSON loc
      , case loggedMessageLogSource of
          Nothing -> Nothing
          Just logSource -> Just $ "source" .@ logSource
      , case loggedMessageThreadContext of
          [] -> Nothing
          pairs -> Just $ "context" .@ Aeson.object pairs
      , Just $ "message" .@ messageToJSON loggedMessageMessage
      ]
    where
    locToJSON :: Loc -> Value
    locToJSON loc =
      Aeson.object
        [ "package" .@ loc_package
        , "module" .@ loc_module
        , "file" .@ loc_filename
        , "line" .@ fst loc_start
        , "char" .@ snd loc_start
        ]
      where
      Loc { loc_filename, loc_package, loc_module, loc_start } = loc

    messageToJSON :: Message -> Value
    messageToJSON (messageText :# messageMeta) =
      Aeson.object $ Maybe.catMaybes
        [ Just $ "text" .@ messageText
        , case messageMeta of
            [] -> Nothing
            pairs -> Just $ "meta" .@ Aeson.object pairs
        ]

    LoggedMessage
      { loggedMessageTimestamp
      , loggedMessageLevel
      , loggedMessageLoc
      , loggedMessageLogSource
      , loggedMessageThreadContext
      , loggedMessageMessage
      } = loggedMessage

  toEncoding loggedMessage = logItemEncoding logItem
    where
    logItem =
      LogItem
        { logItemTimestamp = loggedMessageTimestamp
        , logItemLoc = Maybe.fromMaybe Logger.defaultLoc loggedMessageLoc
        , logItemLogSource = Maybe.fromMaybe "" loggedMessageLogSource
        , logItemLevel = loggedMessageLevel
        , logItemThreadContext = loggedMessageThreadContext
        , logItemMessageEncoding = messageEncoding loggedMessageMessage
        }

    LoggedMessage
      { loggedMessageTimestamp
      , loggedMessageLevel
      , loggedMessageLoc
      , loggedMessageLogSource
      , loggedMessageThreadContext
      , loggedMessageMessage
      } = loggedMessage

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
-- >   [ "bloorp" .@ (42 :: Int)
-- >   , "bonk" .@ ("abc" :: Text)
-- >   ]
--
-- The mnemonic for the ':#' constructor is that the @#@ symbol is sometimes
-- referred to as a hash, a JSON object can be thought of as a hash map, and
-- so with @:#@ (and enough squinting), we are @cons@-ing a textual message onto
-- a JSON object. Yes, this mnemonic isn't well-typed, but hopefully it still
-- helps!
--
-- @since 0.1.0.0
data Message = Text :# [Pair]
  deriving stock (Eq, Generic, Ord, Show)
infixr 5 :#

instance IsString Message where
  fromString string = Text.pack string :# []

-- | Thread-safe, global 'Store' that captures the thread context of messages.
--
-- Note that there is a bit of somewhat unavoidable name-overloading here: this
-- binding is called 'threadContextStore' because it stores the thread context
-- (i.e. @ThreadContext@/@MDC@ from Java land) for messages. It also just so
-- happens that the 'Store' type comes from the @context@ package, which is a
-- package providing thread-indexed storage of arbitrary context values. Please
-- don't hate the player!
--
-- @since 0.1.0.0
threadContextStore :: Store (HashMap Key Value)
threadContextStore =
  IO.Unsafe.unsafePerformIO
    $ Context.newStore Context.noPropagation
    $ Just
    $ HashMap.empty
{-# NOINLINE threadContextStore #-}

-- | 'OutputOptions' is for use with
-- 'Control.Monad.Logger.Aeson.defaultOutputWith' and enables us to configure
-- the JSON output produced by this library.
--
-- We can get a hold of a value of this type via
-- 'Control.Monad.Logger.Aeson.defaultOutputOptions'.
--
-- @since 0.1.0.0
data OutputOptions = OutputOptions
  { outputAction :: LogLevel -> BS8.ByteString -> IO ()
  , -- | Controls whether or not the thread ID is included in each log message's
    -- thread context.
    --
    -- Default: 'True'
    outputIncludeThreadId :: Bool
  , -- | Allows for setting a "base" thread context, i.e. a set of 'Pair' that
    -- will always be present in log messages.
    --
    -- If we subsequently use 'Control.Monad.Logger.Aeson.withThreadContext' to
    -- register some thread context for our messages, if any of the keys in
    -- those 'Pair' values overlap with the "base" thread context, then the
    -- overlapped 'Pair' values in the "base" thread context will be overridden
    -- for the duration of the action provided to
    -- 'Control.Monad.Logger.Aeson.withThreadContext'.
    --
    -- Default: 'mempty'
    outputBaseThreadContext :: [Pair]
  }

defaultLogStrBS
  :: UTCTime
  -> [Pair]
  -> Loc
  -> LogSource
  -> LogLevel
  -> LogStr
  -> BS8.ByteString
defaultLogStrBS now threadContext loc logSource logLevel logStr =
  LBS.toStrict
    $ defaultLogStrLBS now threadContext loc logSource logLevel logStr

defaultLogStrLBS
  :: UTCTime
  -> [Pair]
  -> Loc
  -> LogSource
  -> LogLevel
  -> LogStr
  -> LBS8.ByteString
defaultLogStrLBS now threadContext loc logSource logLevel logStr =
  Aeson.encodingToLazyByteString $ logItemEncoding logItem
  where
  logItem :: LogItem
  logItem =
    case LBS8.uncons logStrLBS of
      Nothing ->
        mkLogItem
          $ messageEncoding
          $ Text.empty :# []
      Just (c, lbs) ->
        -- If the first character of the log string is a XON byte, then we
        -- assume the log string (minus the XON byte) is an encoded 'Message'.
        if c == xonChar then
          mkLogItem
            $ Aeson.unsafeToEncoding
            $ Builder.lazyByteString lbs
        -- Otherwise, we make no assumptions of the log string. We simply decode
        -- to text and use this text in a metadata-less 'Message'.
        else
          mkLogItem
            $ messageEncoding
            $ decodeLenient logStrLBS :# []

  mkLogItem :: Encoding -> LogItem
  mkLogItem messageEnc =
    LogItem
      { logItemTimestamp = now
      , logItemLoc = loc
      , logItemLogSource = logSource
      , logItemLevel = logLevel
      , logItemThreadContext = threadContext
      , logItemMessageEncoding = messageEnc
      }

  decodeLenient =
    Text.Encoding.decodeUtf8With Text.Encoding.Error.lenientDecode
      . LBS.toStrict

  logStrLBS = Builder.toLazyByteString logStrBuilder
  LogStr _ logStrBuilder = logStr

-- | Sneakiness ensues.
logCS
  :: (MonadLogger m)
  => CallStack
  -> LogSource
  -> LogLevel
  -> Message
  -> m ()
logCS cs logSource logLevel msg =
  monadLoggerLog (locFromCS cs) logSource logLevel
    $ xonCharLogStr <> messageToLogStr msg

xonCharLogStr :: LogStr
xonCharLogStr = toLogStr $ LBS8.singleton $ xonChar

xonChar :: Char
xonChar = Char.chr 17

data LogItem = LogItem
  { logItemTimestamp :: UTCTime
  , logItemLoc :: Loc
  , logItemLogSource :: LogSource
  , logItemLevel :: LogLevel
  , logItemThreadContext :: [Pair]
  , logItemMessageEncoding :: Encoding
  }

logItemEncoding :: LogItem -> Encoding
logItemEncoding logItem =
  Aeson.pairs $
    (Aeson.pairStr "time" $ Aeson.toEncoding logItemTimestamp)
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
      <> ( if null logItemThreadContext then
             mempty
           else
             Aeson.pairStr "context" $ pairsEncoding logItemThreadContext
         )
      <> (Aeson.pairStr "message" logItemMessageEncoding)
  where
  LogItem
    { logItemTimestamp
    , logItemLoc
    , logItemLogSource
    , logItemLevel
    , logItemThreadContext
    , logItemMessageEncoding
    } = logItem

messageToLogStr :: Message -> LogStr
messageToLogStr = toLogStr . Aeson.encodingToLazyByteString . messageEncoding

messageEncoding :: Message -> Encoding
messageEncoding  = Aeson.pairs . messageSeries

messageSeries :: Message -> Series
messageSeries message =
  "text" .@ messageText
    <> ( if null messageMeta then
           mempty
         else
           Aeson.pairStr "meta" $ pairsEncoding messageMeta
       )
  where
  messageText :# messageMeta = message

pairsEncoding :: [Pair] -> Encoding
pairsEncoding = Aeson.pairs . pairsSeries

pairsSeries :: [Pair] -> Series
pairsSeries = mconcat . fmap (uncurry (.@))

levelEncoding :: LogLevel -> Encoding
levelEncoding = Aeson.text . logLevelToText

logLevelToText :: LogLevel -> Text
logLevelToText = \case
  LevelDebug -> "debug"
  LevelInfo -> "info"
  LevelWarn -> "warn"
  LevelError -> "error"
  LevelOther otherLevel -> otherLevel

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
                 _            -> Logger.defaultLoc

-- | Not exported from 'monad-logger', so copied here.
isDefaultLoc :: Loc -> Bool
isDefaultLoc (Loc "<unknown>" "<unknown>" "<unknown>" (0,0) (0,0)) = True
isDefaultLoc _ = False

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with care.
