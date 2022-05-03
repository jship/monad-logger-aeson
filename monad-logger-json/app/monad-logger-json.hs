{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main
  ( main
  ) where

import Control.Monad.Logger.CallStack.JSON
import Data.Aeson ((.=))
import Data.Text (Text)

-- $ stack run monad-logger-json | jq
-- {
--   "timestamp": "2022-05-03T00:22:47.0859033Z",
--   "level": "info",
--   "message": {
--     "text": "some message text"
--   }
-- }
-- {
--   "timestamp": "2022-05-03T00:22:47.0859889Z",
--   "level": "debug",
--   "message": {
--     "text": "some message text"
--   }
-- }
-- {
--   "timestamp": "2022-05-03T00:22:47.0859985Z",
--   "level": "debug",
--   "location": {
--     "package": "main",
--     "module": "Main",
--     "file": "app/monad-logger-json.hs",
--     "line": 112,
--     "char": 5
--   },
--   "message": {
--     "text": "Some log message without metadata"
--   }
-- }
-- {
--   "timestamp": "2022-05-03T00:22:47.0860342Z",
--   "level": "warn",
--   "location": {
--     "package": "main",
--     "module": "Main",
--     "file": "app/monad-logger-json.hs",
--     "line": 115,
--     "char": 5
--   },
--   "message": {
--     "text": "foo bar baz"
--   }
-- }
-- {
--   "timestamp": "2022-05-03T00:22:47.0860481Z",
--   "level": "warn",
--   "location": {
--     "package": "main",
--     "module": "Main",
--     "file": "app/monad-logger-json.hs",
--     "line": 116,
--     "char": 5
--   },
--   "message": {
--     "text": "quux stuff",
--     "meta": {
--       "bloorp": 42,
--       "bonk": "abc"
--     }
--   }
-- }
-- {
--   "timestamp": "2022-05-03T00:22:47.0860773Z",
--   "level": "warn",
--   "location": {
--     "package": "main",
--     "module": "Main",
--     "file": "app/monad-logger-json.hs",
--     "line": 120,
--     "char": 5
--   },
--   "message": {
--     "text": "quux stuff 2",
--     "meta": {
--       "bar": null,
--       "foo": 42
--     }
--   }
-- }
main :: IO ()
main = do
  runStdoutLoggingT do
    -- We can use functions from 'monad-logger' directly if we want, though we
    -- won't be able to log pairs with these functions.
    logInfoN "some message text"
    logDebugN "some message text"

    -- Pretend 'logSomeJSON' was instead some standard 'monad-logger' name
    -- like 'logInfoNS' (it isn't right now out of laziness/quick testing).
    --
    -- We can leverage the 'IsString' instance of 'Message' in the case when
    -- we don't have pairs.
    logDebug "Some log message without metadata"

    -- When we do have pairs, we can just tack on the pairs list with ':#'.
    logWarn $ "foo bar baz" :# []
    logWarn $ "quux stuff" :#
      [ "bloorp" .= (42 :: Int)
      , "bonk" .= ("abc" :: Text)
      ]
    logWarn $ "quux stuff 2" :#
      [ "foo" .= Just @Int 42
      , "bar" .= Nothing @Int
      ]
