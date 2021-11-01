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


--------------------------------------------------------------------------------

endpoint = "http://178.20.158.25:35554/v1"

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
reqGetData :: Text -> RPC ()
reqGetData url = method "get" $ Named [("jsonrpc", String "2.0"), ("url", String url)]

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
reqGetMetrics :: Text -> RPC ()
reqGetMetrics url = method "metrics" $ List [String url]


--------------------------------------------------------------------------------
-- Faucet

-- |
--
reqFaucet :: Text -> RPC ()
reqFaucet url = method "faucet" $ List [String url]


--------------------------------------------------------------------------------
-- Credits

-- |
--
reqAddCredits :: Text -> RPC ()
reqAddCredits url = method "add-credits" $ List [String url]


main = do
  let s = weakSession (traceSendAPI "" $ clientSendAPI endpoint)
  h <-
    send s $ do
      h <- reqGetData "acc://9c549cbba290efeb8029184caac3d36c5bfcacb361a29282/ACME"
      return h
  print h
