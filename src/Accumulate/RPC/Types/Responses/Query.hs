{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Accumulate.RPC.Types.Responses.Query
    ( VersionResponse (..)
    , MainChain (..)
    , Data (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)

data VersionResponse = VersionResponse
    { merkleStateVersionResponse :: MainChain
    , chainIDVersionResponse :: Text
    , versionResponseDataVersionResponse :: Data
    , versionResponseTypeVersionResponse :: Text
    , mainChainVersionResponse :: MainChain
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

decodeTopLevel :: ByteString -> Maybe VersionResponse
decodeTopLevel = decode

instance ToJSON VersionResponse where
    toJSON (VersionResponse merkleStateVersionResponse chainIDVersionResponse versionResponseDataVersionResponse versionResponseTypeVersionResponse mainChainVersionResponse) =
        object
        [ "merkleState" .= merkleStateVersionResponse
        , "chainId" .= chainIDVersionResponse
        , "data" .= versionResponseDataVersionResponse
        , "type" .= versionResponseTypeVersionResponse
        , "mainChain" .= mainChainVersionResponse
        ]

instance FromJSON VersionResponse where
    parseJSON (Object v) = VersionResponse
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
