{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import Control.Monad.Logger.Aeson

doStuff :: (MonadLogger m) => Int -> m ()
doStuff x = do
  logDebug $ "Doing stuff" :# ["x" .@ x]

main :: IO ()
main = do
  runStdoutLoggingT do
    doStuff 42
    logInfo "Done"
