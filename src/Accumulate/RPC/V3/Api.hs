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
import           Control.Exception                                   (bracket)
import           Control.Monad.IO.Class
import           Control.Remote.Monad.JSON
import           Control.Remote.Monad.JSON.Client
import           Control.Remote.Monad.JSON.Router
import           Control.Remote.Monad.JSON.Trace
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe
import           Data.Text                                           as T
import           GHC.Exts
import           Network.Socket                                      (HostName,
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

import           Accumulate.RPC.JsonRpc                              (JsonRpcT,
                                                                      runJsonRpcT)
import           Accumulate.RPC.Types.ApiDataResponse
import           Accumulate.RPC.Types.Responses.Query
import           Accumulate.RPC.Types.Responses.QueryDirectory
import           Accumulate.RPC.Types.Responses.QueryLiteIdentity
import           Accumulate.RPC.Types.Responses.Version
import           Accumulate.RPC.V3.Types.ApiFindServiceResponse
import           Accumulate.RPC.V3.Types.ApiMajorBlocksRangeResponse
import           Accumulate.RPC.V3.Types.ApiMetricsResponse
import           Accumulate.RPC.V3.Types.ApiMinorBlockResponse
import           Accumulate.RPC.V3.Types.ApiMinorBlocksRangeResponse
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

reqGetFindService :: RPC FindServiceResponse
reqGetFindService = method "find-service" $ Named [ ("network", "MainNet")
                                                  , ("known", Bool True)
                                                  , ("service", Object $
                                                                  fromList
                                                                    [ ("type"    , String "consensus") -- equal to reqGetConsensusStatus
                                                                    , ("argument", String "directory")
                                                                    ]
                                                    )

                                                  ]


-- | Rerieves Minor blocks for specified range
--      to avoid useless records use ("omitEmpty", Bool True) in query params
reqQueryMinorBlocks :: Text -> Int -> Int -> RPC MinorBlocksRangeResponse
reqQueryMinorBlocks url start count =
  method "query"
    $ Named [ ("scope", String url)
            , ("query", Object $ fromList
                [ ("queryType" , String "block")
                , ("minorRange", Object $ fromList
                                 [ ("start", Number $ fromIntegral start)
                                 , ("count", Number $ fromIntegral count)
                                 , ("expand", Bool True)
                                 ])
                ])
            ]

reqQueryMinorBlockByHeight :: Text -> Int -> RPC MinorBlockResponse
reqQueryMinorBlockByHeight url height =
  method "query"
    $ Named [ ("scope", String url)
            , ("query", Object $ fromList  [ ("queryType" , String "block")
                                           , ("minor",  Number $ fromIntegral height)
                                           ])
            ]


reqQueryMajorBlocks :: Text -> Int -> Int -> RPC MajorBlocksRangeResponse
reqQueryMajorBlocks url start count =
  method "query"
    $ Named [ ("scope", String url)
            , ("query", Object $ fromList
                [ ("queryType" , String "block")
                , ("majorRange", Object $ fromList
                                 [ ("start", Number $ fromIntegral start)
                                 , ("count", Number $ fromIntegral count)
                                 , ("expand", Bool True)
                                 ])
                ])
            ]

-- Retrieves last 50 major blocks
reqGetLastMajorBlocks :: Text -> RPC MajorBlocksRangeResponse
reqGetLastMajorBlocks url  =
  method "query"
    $ Named [ ("scope", String url)
            , ("query", Object $ fromList
                [ ("queryType" , String "block")
                , ("majorRange", Object $ fromList
                                 [ ("fromEnd", Bool True)
                                 ])
                ])
            ]

reqQueryMajorBlockByHeight :: Text -> Int -> RPC MinorBlockResponse
reqQueryMajorBlockByHeight url height =
  method "query"
    $ Named [ ("scope", String url)
            , ("query", Object $ fromList  [ ("queryType" , String "block")
                                           , ("major",  Number $ fromIntegral height)
                                           ])
            ]


--------------------------------------------------------------------------------
-- Credits

-- |
--
reqAddCredits :: Text -> RPC ()
reqAddCredits url = method "add-credits" $ List [String url]

--------------------------------------------------------------------------------
-- Utility functions

getCurrentHeightMajor :: IO Accumulate.RPC.V3.Types.ApiMajorBlocksRangeResponse.Record
getCurrentHeightMajor = do
  let s = weakSession (traceSendAPI "" $ clientSendAPI endpoint)
  (mr) <-
    send s $ do
      mr <- reqGetLastMajorBlocks "acc://bvn-Apollo.acme"
      return mr

  let currentHeightMajor = Prelude.last $ fromMaybe [] $ recordsMajorBlocksRangeResponse $ mr

  return $ currentHeightMajor


--------------------------------------------------------------------------------
-- TESTING

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

      -- q1 <- reqGetMetricsStatus ""

      --q1 <- reqGetFindService
      --q2 <- reqQueryMinorBlocks "acc://bvn-Apollo.acme" 1 100
      --q3 <- reqQueryMinorBlockByHeight "acc://bvn-Chandrayaan.acme" 15927876 -- "acc://bvn-Apollo.acme"
      q3 <- reqGetLastMajorBlocks "acc://bvn-Apollo.acme"

      let
           q1 = ""
           q2 = ""
      return (v, q1, q2, q3)

  putStrLn "---------------------------------------------------"
  print $ show $ v

  putStrLn "-- STATUS ------------------------------------------------"
  print $ show $ networkNetworkStatusResponse $ v

  putStrLn "-- GLOBALS -----------------"
  print $ show $ globalsNetworkStatusResponse $ v

  putStrLn "-- FEE SCHEDULE ---------"
  print $ createIdentitySlidingFeeSchedule $ feeScheduleGlobals $ globalsNetworkStatusResponse $ v

  putStrLn "-- CONSENSUS STATUS ---------"
  print $ show $ q1

  putStrLn "---------------------------------------------------"
  putStrLn "-- MINOR BLOCKS ---------"
  --print $ Prelude.length $ fromJust $ recordsMinorBlocksRangeResponse q2

  putStrLn "---------------------------------------------------"
  putStrLn "-- MAJOR BLOCKS ---------"
  print $ show $ q3

  putStrLn "---------------------------------------------------"
  putStrLn "-- MAJOR HEIGHT ---------"
  let currentHeightMajor = Prelude.last $ fromMaybe [] $ recordsMajorBlocksRangeResponse $ q3
  print $ show $ currentHeightMajor
