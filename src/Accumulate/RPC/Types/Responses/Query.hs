{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Accumulate.RPC.Types.Responses.Query
    ( QueryResponse (..)
    , MainChain (..)
    , Data (..)
    , decodeResponseQuery
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)

data QueryResponse = QueryResponse
    { merkleStateQueryResponse :: MainChain
    , chainIDQueryResponse :: Text
    , queryResponseDataQueryResponse :: Data
    , queryResponseTypeQueryResponse :: Text
    , mainChainQueryResponse :: MainChain
    } deriving (Show)

data MainChain = MainChain
    { heightMainChain :: Int
    , rootsMainChain :: [(Maybe Text)]
    , countMainChain :: Int
    } deriving (Show)

data Data = Data
    { managerKeyBookData :: Text
    , urlData :: Text
    , balanceData :: Text
    , keyBookData :: Text
    , tokenURLData :: Text
    , dataTypeData :: Text
    , creditBalanceData :: Text
    , nonceData :: Int
    } deriving (Show)

decodeResponseQuery :: ByteString -> Maybe QueryResponse
decodeResponseQuery = decode

instance ToJSON QueryResponse where
    toJSON (QueryResponse merkleStateQueryResponse chainIDQueryResponse queryResponseDataQueryResponse queryResponseTypeQueryResponse mainChainQueryResponse) =
        object
        [ "merkleState" .= merkleStateQueryResponse
        , "chainId" .= chainIDQueryResponse
        , "data" .= queryResponseDataQueryResponse
        , "type" .= queryResponseTypeQueryResponse
        , "mainChain" .= mainChainQueryResponse
        ]

instance FromJSON QueryResponse where
    parseJSON (Object v) = QueryResponse
        <$> v .: "merkleState"
        <*> v .: "chainId"
        <*> v .: "data"
        <*> v .: "type"
        <*> v .: "mainChain"

instance ToJSON MainChain where
    toJSON (MainChain heightMainChain rootsMainChain countMainChain) =
        object
        [ "height" .= heightMainChain
        , "roots" .= rootsMainChain
        , "count" .= countMainChain
        ]

instance FromJSON MainChain where
    parseJSON (Object v) = MainChain
        <$> v .: "height"
        <*> v .: "roots"
        <*> v .: "count"

instance ToJSON Data where
    toJSON (Data managerKeyBookData urlData balanceData keyBookData tokenURLData dataTypeData creditBalanceData nonceData) =
        object
        [ "managerKeyBook" .= managerKeyBookData
        , "url" .= urlData
        , "balance" .= balanceData
        , "keyBook" .= keyBookData
        , "tokenUrl" .= tokenURLData
        , "type" .= dataTypeData
        , "creditBalance" .= creditBalanceData
        , "nonce" .= nonceData
        ]

instance FromJSON Data where
    parseJSON (Object v) = Data
        <$> v .: "managerKeyBook"
        <*> v .: "url"
        <*> v .: "balance"
        <*> v .: "keyBook"
        <*> v .: "tokenUrl"
        <*> v .: "type"
        <*> v .: "creditBalance"
        <*> v .: "nonce"
