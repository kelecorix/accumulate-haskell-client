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
import           Accumulate.RPC.Types.Responses.Version

--------------------------------------------------------------------------------

endpoint = "https://v3.testnet.accumulatenetwork.io/v2"

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


-- Options to convert internal types into Aeson values
-- $w Named [("jsonrpc", String "2.0"), ("id", toJSON (0::Int))]

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

-- | Get infromation about token
--
reqQuery :: Text -> RPC ()
reqQuery url = method "query" $ Named [("url", String url)]


-- | Get information about token transaction
--
reqQueryTransaction :: Text -> RPC ()
reqQueryTransaction txid = method "query-tx" $ List [String txid]

-- |
--
reqQueryDirectory :: Text -> RPC ()
reqQueryDirectory url = method "query-directory" $ List [String url]

--------------------------------------------------------------------------------
-- Metrics

-- |
--
-- reqGetMetrics :: Text -> Text -> RPC APIDataMetricsResponse
-- reqGetMetrics metricName duration =
--   method "metrics" $ Named [ ("metric"  , String metricName)
--                            , ("duration", String duration)
--                            ]

reqGetVersion :: RPC VersionResponse
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

--------------------------------------------------------------------------------
main = do
  let s = weakSession (traceSendAPI "" $ clientSendAPI endpoint)
  (m,v) <-
    send s $ do
      --h <- reqGetData "acc://c9359900016daa23da0f4c07e66be42c398fe2b10017cecb/ACME"
      --t <- reqGetTokenTx ""
      --m <- reqGetMetrics "tps" "1h"
      v  <- reqGetVersion
      q1 <- reqQuery "acc://5d21072c5d44111fcd3cbe25161f5e143498b56266dc1cd8/acme"
      return (v, q1)
  putStrLn "-----------"
  print $ show $ m
  putStrLn "-----------"
  print $ show $ v
