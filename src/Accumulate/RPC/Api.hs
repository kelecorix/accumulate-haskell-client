{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Accumulate.RPC.Api
  ( runTCPClient
  , reqGetData
  ) where

import           Control.Concurrent
import           Control.Exception                     (bracket)
import           Control.Monad.IO.Class
import           Control.Remote.Monad.JSON
import           Control.Remote.Monad.JSON.Client
import           Control.Remote.Monad.JSON.Router
import           Control.Remote.Monad.JSON.Trace
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text                             as T
import           Network.Socket                        (HostName, ServiceName,
                                                        SocketType (Stream),
                                                        addrAddress, addrFamily,
                                                        addrProtocol,
                                                        addrSocketType, close,
                                                        connect, defaultHints,
                                                        getAddrInfo, socket)

import           Accumulate.RPC.JsonRpc                    (JsonRpcT, runJsonRpcT)
import           Accumulate.RPC.Types.ApiDataResponse
import           Accumulate.RPC.Types.ApiDataMetricsResponse
import           Accumulate.RPC.Types.ApiDataVersionResponse

--------------------------------------------------------------------------------

endpoint = "http://95.141.37.250:35554/v1"

runTCPClient :: HostName -> ServiceName -> JsonRpcT IO a -> IO a
runTCPClient host port f = do
  addr <- resolve host port
  bracket (open addr) close talk
  where
    resolve host' port' = do
      let hints = defaultHints {addrSocketType = Stream}
      addr:_ <- getAddrInfo (Just hints) (Just host') (Just port')
      return addr
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock $ addrAddress addr
      return sock
    talk sock = runJsonRpcT sock f


-- $ Named [("jsonrpc", String "2.0"), ("id", toJSON (0::Int))]

--------------------------------------------------------------------------------
-- Data retrieval methods

-- |  getData returns Accumulate Object by URL
--
reqGetData :: Text -> RPC APIDataResponse
reqGetData url = method "get" $ Named [("url", String url)] --  Named [("wait", Bool False), ("url", String url)]

-- |  GetDirectory returns ADI directory entries
--
reqGetDirectory :: Text -> RPC ()
reqGetDirectory url = method "get-directory" $ List [String url]

--------------------------------------------------------------------------------
-- ADIs

-- |
--
reqGetADI :: Text -> RPC ()
reqGetADI url = method "adi" $ List [String url]

-- |
--
reqCreateADI :: Text -> RPC ()
reqCreateADI url = method "adi-create" $ List [String url]


--------------------------------------------------------------------------------
-- Key Management

-- |
--
reqGetKeyBook :: Text -> RPC ()
reqGetKeyBook url = method "sig-spec-group" $ List [String url]


--------------------------------------------------------------------------------
-- Tokens

-- |
--
reqGetToken :: Text -> RPC ()
reqGetToken url = method "token" $ List [String url]


--------------------------------------------------------------------------------
-- Metrics

-- |
--
reqGetMetrics :: Text -> Text -> RPC APIDataMetricsResponse
reqGetMetrics metricName duration =
  method "metrics" $ Named [ ("metric"  , String metricName)
                           , ("duration", String duration)
                           ]

reqGetVersion :: RPC APIDataVersionResponse
reqGetVersion = method "version" $ List []


--------------------------------------------------------------------------------
-- Faucet

-- |
--
reqFaucet :: Text -> RPC ()
reqFaucet url = method "faucet" $ Named [("url", String url)]


--------------------------------------------------------------------------------
-- Credits

-- |
--
reqAddCredits :: Text -> RPC ()
reqAddCredits url = method "add-credits" $ List [String url]


main = do
  let s = weakSession (traceSendAPI "" $ clientSendAPI endpoint)
  (m,v) <-
    send s $ do
      --h <- reqGetData "acc://c9359900016daa23da0f4c07e66be42c398fe2b10017cecb/ACME"
      m <- reqGetMetrics "tps" "1h"
      v <- reqGetVersion
      return (m, v)
  putStrLn "-----------"
  print $ show $ m
  putStrLn "-----------"
  print $ show $ v
