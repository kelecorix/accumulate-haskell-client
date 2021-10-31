{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent          (forkIO, threadDelay)
import           Control.Concurrent.STM.TVar
import           Control.Monad               (forever)
import qualified Data.Text                   as T
import           Data.Time.Clock
import           System.Environment

import qualified Accumulate.RPC.Api              as AcmeApi
import qualified Accumulate.RPC.Types            as AcmeApi

--------------------------------------------------------------------------------
main :: IO ()
main = do
  let delay = 60000 -- 1 request per minute available
  a <- getArgs
  timeVar <- newTVarIO (delay * 1000)
  case a of
    ["-t"] -> do
      -- read token from file
      return ()
    otherwise -> do
      putStrLn $ "Accumulate | Establishing connection"
      putStrLn $ "Accumulate | Getting last entries\n"

      -- let s = weakSession (traceSendAPI "" $ clientSendAPI endpoint)
      -- h <-
      --   send s $ do
      --     h <- reqGetData "acc://9c549cbba290efeb8029184caac3d36c5bfcacb361a29282/ACME"
      --     return h
      -- print h
      return ()
