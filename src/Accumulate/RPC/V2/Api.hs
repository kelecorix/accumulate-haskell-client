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

module Accumulate.RPC.V2.Api
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
import           Accumulate.RPC.Types.ApiMinorBlocksResponse
import           Accumulate.RPC.Types.ApiNetworkDescribeResponse
import           Accumulate.RPC.Types.Responses.Query
import           Accumulate.RPC.Types.Responses.QueryDirectory
import           Accumulate.RPC.Types.Responses.QueryLiteIdentity
import           Accumulate.RPC.Types.Responses.Version

--------------------------------------------------------------------------------

endpoint = "https://mainnet.accumulatenetwork.io/v2"

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


-- |  GetDirectory returns ADI directory entries
--
reqQueryMinorBlocks :: Text -> Int -> Int -> RPC APIMinorBlocksResponse
reqQueryMinorBlocks url start count =
  method "query-minor-blocks"
    $ Named [ ("url"   , String url)
            , ("start" , Number (fromIntegral start))
            , ("count" , Number (fromIntegral count))
            , ("TxFetchMode", String "Expand")
            , ("BlockFilterMode", String "ExcludeNone")
            ]


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
-- Metrics via Describe

-- |
--
reqDescribe :: RPC APINetworkDescribeResponse
reqDescribe = method "describe" $ List []

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
  --(v, q1, q2, q3, d) <-
  (v, q1, d) <-
    send s $ do
      let v = "undefined"
      -- v  <- reqGetVersion -- NB: after v1.2 started to return empty object
      q1 <- reqQuery "acc://kompendium.acme/tokens"
      -- "acc://12d3ab9ed87ab6b8755163d53ba475b2d7976b1e14b70f2a/acme"
      --q2 <- reqQueryLiteIdentity "acc://12d3ab9ed87ab6b8755163d53ba475b2d7976b1e14b70f2a"
      --q3 <- reqQueryLiteIdentityAsDir "acc://12d3ab9ed87ab6b8755163d53ba475b2d7976b1e14b70f2a"
      --q2 <- reqQueryTransaction "1748e91a5bb57d6deabc341659828800694aaaae8178db5d5e8885d47431cbe1"
      --q3 <- reqQueryDirectory "acc://kompendium.acme"
      d  <- reqDescribe
      --return (v, q1, q2, q3, d)
      return(v, q1, d)

  putStrLn "---------------------------------------------------"
  print $ show $ v
  putStrLn "//////---------------------------------------"
  print $ show $ q1
  -- putStrLn "-----------"
  -- print $ show $ q2
  -- putStrLn "-----------"
  -- print $ show $ q3
  putStrLn "//////---------------------------------------"
  print $ show $ d

-- curl --location --request GET 'https://mainnet.accumulatenetwork.io/v2' \
-- --data '{
--     "jsonrpc": "2.0",
--     "method": "query-minor-blocks",
--     "params": {
--         "url": "acc://bvn-Apollo.acme",
--         "count": 100,
--         "start": 2
--     },
--     "id": 1822
-- }'

parseMinorBlocks = do
  let s = weakSession (traceSendAPI "" $ clientSendAPI endpoint)
  (v, r) <-
    send s $ do
      let v = "undefined"
      -- v <- reqGetVersion
      -- r <- reqQueryMinorBlocks "acc://bvn-Harpo.acme" 1 100  -- Testnet
      r <- reqQueryMinorBlocks "acc://bvn-Apollo.acme" 2 100 -- chandrayaan
      return (v,r)

  putStrLn "---------------------------------------------------"
  print $ show $ v
  putStrLn "-----------"
  print $ show $ r

  mapM_ (\blck -> do
            putStrLn "---------------------------------------------------"
            putStrLn $ show $ blockIndexItem blck
            putStrLn $ show $ fromMaybe 0 $ txCountItem blck
        )
        $ fromMaybe [] $ itemsAPIMinorBlocksResponse r
