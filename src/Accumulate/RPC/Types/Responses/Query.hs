{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Accumulate.RPC.Types.Responses.Query
    ( QueryResponse (..)
    , Chain (..)
    , MainChain (..)
    , Data (..)
    , decodeQueryResponse
    ) where

import           Data.Aeson
import           Data.Aeson.Types     (emptyObject)
import           Data.ByteString.Lazy (ByteString)
import           Data.HashMap.Strict  (HashMap)
import           Data.Text            (Text)

data QueryResponse = QueryResponse
    { merkleStateQueryResponse       :: MainChain
    , queryResponseTypeQueryResponse :: Text
    , chainsQueryResponse            :: [Chain]
    , chainIDQueryResponse           :: Text
    , queryResponseDataQueryResponse :: Data
    , mainChainQueryResponse         :: MainChain
    } deriving (Show)

data Chain = Chain
    { nameChain      :: Text
    , chainTypeChain :: Text
    , countChain     :: Int
    , rootsChain     :: [(Maybe Text)]
    , heightChain    :: Int
    } deriving (Show)

data MainChain = MainChain
    { countMainChain  :: Int
    , rootsMainChain  :: [(Maybe Text)]
    , heightMainChain :: Int
    } deriving (Show)

data Data = Data
    { dataTypeData :: Text
    , urlData      :: Text
    , tokenURLData :: Text
    , balanceData  :: Text
    } deriving (Show)

decodeQueryResponse :: ByteString -> Maybe QueryResponse
decodeQueryResponse = decode

instance ToJSON QueryResponse where
    toJSON (QueryResponse merkleStateQueryResponse queryResponseTypeQueryResponse chainsQueryResponse chainIDQueryResponse queryResponseDataQueryResponse mainChainQueryResponse) =
        object
        [ "merkleState" .= merkleStateQueryResponse
        , "type" .= queryResponseTypeQueryResponse
        , "chains" .= chainsQueryResponse
        , "chainId" .= chainIDQueryResponse
        , "data" .= queryResponseDataQueryResponse
        , "mainChain" .= mainChainQueryResponse
        ]

instance FromJSON QueryResponse where
    parseJSON (Object v) = QueryResponse
        <$> v .: "merkleState"
        <*> v .: "type"
        <*> v .: "chains"
        <*> v .: "chainId"
        <*> v .: "data"
        <*> v .: "mainChain"

instance ToJSON Chain where
    toJSON (Chain nameChain chainTypeChain countChain rootsChain heightChain) =
        object
        [ "name" .= nameChain
        , "type" .= chainTypeChain
        , "count" .= countChain
        , "roots" .= rootsChain
        , "height" .= heightChain
        ]

instance FromJSON Chain where
    parseJSON (Object v) = Chain
        <$> v .: "name"
        <*> v .: "type"
        <*> v .: "count"
        <*> v .: "roots"
        <*> v .: "height"

instance ToJSON MainChain where
    toJSON (MainChain countMainChain rootsMainChain heightMainChain) =
        object
        [ "count" .= countMainChain
        , "roots" .= rootsMainChain
        , "height" .= heightMainChain
        ]

instance FromJSON MainChain where
    parseJSON (Object v) = MainChain
        <$> v .: "count"
        <*> v .: "roots"
        <*> v .: "height"

instance ToJSON Data where
    toJSON (Data dataTypeData urlData tokenURLData balanceData) =
        object
        [ "type" .= dataTypeData
        , "url" .= urlData
        , "tokenUrl" .= tokenURLData
        , "balance" .= balanceData
        ]

instance FromJSON Data where
    parseJSON (Object v) = Data
        <$> v .: "type"
        <*> v .: "url"
        <*> v .: "tokenUrl"
        <*> v .: "balance"
