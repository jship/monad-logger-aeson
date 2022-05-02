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
--   "timestamp": "2022-05-02T01:50:32.9352623Z",
--   "level": "info",
--   "source": "some-source",
--   "message": {
--     "text": "some message text"
--   }
-- }
-- {
--   "timestamp": "2022-05-02T01:50:32.9353538Z",
--   "level": "debug",
--   "message": {
--     "text": "some message text"
--   }
-- }
-- {
--   "timestamp": "2022-05-02T01:50:32.9353712Z",
--   "level": "warn",
--   "source": "some-source",
--   "message": {
--     "text": "foo bar"
--   }
-- }
-- {
--   "timestamp": "2022-05-02T01:50:32.9354058Z",
--   "level": "warn",
--   "source": "some-source",
--   "message": {
--     "text": "foo bar baz"
--   }
-- }
-- {
--   "timestamp": "2022-05-02T01:50:32.9354215Z",
--   "level": "warn",
--   "source": "some-source",
--   "message": {
--     "text": "quux stuff",
--     "meta": {
--       "bloorp": 42,
--       "bonk": "abc"
--     }
--   }
-- }
-- {
--   "timestamp": "2022-05-02T01:50:32.9354496Z",
--   "level": "warn",
--   "source": "some-source",
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
    logInfoNS "some-source" "some message text"
    logDebugN "some message text"

    -- Pretend 'logSomeJSON' was instead some standard 'monad-logger' name
    -- like 'logInfoNS' (it isn't right now out of laziness/quick testing).
    --
    -- We can leverage the 'IsString' instance of 'Message' in the case when
    -- we don't have pairs.
    logDebug "foo bar"

    -- When we do have pairs, we can just tack on the pairs list with ':&.
    logWarn $ "foo bar baz" :& []
    logWarn $ "quux stuff" :&
      [ "bloorp" .= (42 :: Int)
      , "bonk" .= ("abc" :: Text)
      ]
    logWarn $ "quux stuff 2" :&
      [ "foo" .= Just @Int 42
      , "bar" .= Nothing @Int
      ]
