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

module Accumulate.RPC.V3.Api
  ( runTCPClient
  , reqGetData
  ) where

import           Control.Concurrent
import           Control.Exception                                (bracket)
import           Control.Monad.IO.Class
import           Control.Remote.Monad.JSON
import           Control.Remote.Monad.JSON.Client
import           Control.Remote.Monad.JSON.Router
import           Control.Remote.Monad.JSON.Trace
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe
import           Data.Text                                        as T
import           Network.Socket                                   (HostName,
                                                                   ServiceName,
                                                                   SocketType (Stream),
                                                                   addrAddress,
                                                                   addrFamily,
                                                                   addrProtocol,
                                                                   addrSocketType,
                                                                   close,
                                                                   connect,
                                                                   defaultHints,
                                                                   getAddrInfo,
                                                                   socket)

import           Accumulate.RPC.JsonRpc                           (JsonRpcT,
                                                                   runJsonRpcT)
import           Accumulate.RPC.Types.ApiDataResponse
import           Accumulate.RPC.Types.Responses.Query
import           Accumulate.RPC.Types.Responses.QueryDirectory
import           Accumulate.RPC.Types.Responses.QueryLiteIdentity
import           Accumulate.RPC.Types.Responses.Version
import           Accumulate.RPC.V3.Types.ApiMetricsResponse
import           Accumulate.RPC.V3.Types.ApiNetworkStatusResponse

--------------------------------------------------------------------------------

endpoint = "https://mainnet.accumulatenetwork.io/v3"

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
-- Faucet

-- |
--
reqFaucet :: Text -> RPC ()
reqFaucet url = method "faucet" $ Named [("url", String url)]

--------------------------------------------------------------------------------
-- Tokens

-- | Get infromation about token
--
reqQuery :: Text -> RPC QueryResponse
reqQuery url = method "query" $ Named [("url", String url)]

reqQueryLiteIdentity :: Text -> RPC QueryResponseLiteIdentity
reqQueryLiteIdentity url =
  method "query" $ Named [("url", String url)]

reqQueryLiteIdentityAsDir :: Text -> RPC QueryResponseLiteIdentity
reqQueryLiteIdentityAsDir url =
  method "query-directory" $ Named [ ("url"   , String url)
                                   , ("expand", Bool True)
                                   , ("expandChains", Bool True)
                                   , ("start", Number 1)
                                   , ("count", Number 20)
                                   ]

reqQueryLiteIdentityAsChain :: Text -> RPC ()
reqQueryLiteIdentityAsChain chainId =
  method "query-chain" $ Named [("chainId", String chainId)]

-- | Get information about token transaction
--
reqQueryTransaction :: Text -> RPC ()
reqQueryTransaction txid = method "query-tx" $ List [String txid]

-- |
--
reqQueryDirectory :: Text -> RPC QueryDirectoryResponse
reqQueryDirectory url = method "query-directory" $ Named [("url", String url)]

reqQueryDirectory' :: Text -> RPC ()
reqQueryDirectory' url = method "query" $ Named [("url", String url)]

--------------------------------------------------------------------------------

reqGetNetworkStatus :: RPC NetworkStatusResponse
reqGetNetworkStatus = method "network-status" $ Named [("", String "")]


reqGetConsensusStatus :: Text -> RPC VersionResponse
reqGetConsensusStatus partition = method "consensus-status" $ Named [("nodeID", String partition)]

reqGetMetricsStatus :: Text -> RPC MetricsResponse
reqGetMetricsStatus p = method "metrics" $ Named [("partition", String p)]

reqGetFindService :: RPC MetricsResponse
reqGetFindService = method "find-service" $ Named [("network", "MainNet")]


-- |  GetDirectory returns ADI directory entries
--
-- reqQueryMinorBlocks :: Text -> Int -> Int -> RPC APIMinorBlocksResponse
-- reqQueryMinorBlocks url start count =
--   method "query-minor-blocks"
--     $ Named [ ("url"   , String url)
--             , ("start" , Number (fromIntegral start))
--             , ("count" , Number (fromIntegral count))
--             , ("TxFetchMode", String "Expand")
--             , ("BlockFilterMode", String "ExcludeNone")
--             ]


--------------------------------------------------------------------------------
-- Credits

-- |
--
reqAddCredits :: Text -> RPC ()
reqAddCredits url = method "add-credits" $ List [String url]

--------------------------------------------------------------------------------
main = do
  let s = weakSession (traceSendAPI "" $ clientSendAPI endpoint)
  (v, q1, q2, q3) <-
    send s $ do
      v  <- reqGetNetworkStatus
      --q1 <- reqQuery "https://explorer.accumulatenetwork.io/acc/kompendium.acme/tokens"
      --q1 <- reqQuery "acc://12d3ab9ed87ab6b8755163d53ba475b2d7976b1e14b70f2a/acme"
      --q2 <- reqQueryLiteIdentity "acc://12d3ab9ed87ab6b8755163d53ba475b2d7976b1e14b70f2a"
      --q3 <- reqQueryLiteIdentityAsDir "acc://12d3ab9ed87ab6b8755163d53ba475b2d7976b1e14b70f2a"
      --q2 <- reqQueryTransaction "1748e91a5bb57d6deabc341659828800694aaaae8178db5d5e8885d47431cbe1"
      --q3 <- reqQueryDirectory "acc://kompendium.acme"

      -- q1 <- reqGetConsensusStatus "12D3KooWQwSDdabKJJao5kB78PymyBaBiPmoBy2B7ft57LbfB7de"
      -- q1 <- reqGetMetricsStatus ""

      --q1 <- reqGetFindService

      let q2 = ""
          q3 = ""
      return (v, q1, q2, q3)

  putStrLn "---------------------------------------------------"
  print $ show $ v
  putStrLn "--------------------------------------------------"
  print $ show $ networkNetworkStatusResponse $ v
  putStrLn "-----------"
  print $ show $ globalsNetworkStatusResponse $ v
  putStrLn "-----------"
  print $ createIdentitySlidingFeeSchedule $ feeScheduleGlobals $ globalsNetworkStatusResponse $ v


-- Small Tasks from Network call
-- 1. construct list of tuples (key-value) that represent validators
-- 2. get list of partitions
