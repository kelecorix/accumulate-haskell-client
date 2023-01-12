{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Accumulate.RPC.Types.Responses.QueryDirectory
    ( QueryDirectoryResponse (..)
    , MainChain (..)
    , Data (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)

data QueryDirectoryResponse = QueryDirectoryResponse
    { queryDirectoryResponseTypeQueryDirectoryResponse :: Text
    , mainChainQueryDirectoryResponse :: MainChain
    , merkleStateQueryDirectoryResponse :: MainChain
    , queryDirectoryResponseDataQueryDirectoryResponse :: Data
    , chainIDQueryDirectoryResponse :: Text
    } deriving (Show)

data MainChain = MainChain
    { heightMainChain :: Int
    , countMainChain :: Int
    , rootsMainChain :: [Text]
    } deriving (Show)

data Data = Data
    { dataTypeData :: Text
    , urlData :: Text
    , keyBookData :: Text
    , managerKeyBookData :: Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe QueryDirectoryResponse
decodeTopLevel = decode

instance ToJSON QueryDirectoryResponse where
    toJSON (QueryDirectoryResponse queryDirectoryResponseTypeQueryDirectoryResponse mainChainQueryDirectoryResponse merkleStateQueryDirectoryResponse queryDirectoryResponseDataQueryDirectoryResponse chainIDQueryDirectoryResponse) =
        object
        [ "type" .= queryDirectoryResponseTypeQueryDirectoryResponse
        , "mainChain" .= mainChainQueryDirectoryResponse
        , "merkleState" .= merkleStateQueryDirectoryResponse
        , "data" .= queryDirectoryResponseDataQueryDirectoryResponse
        , "chainId" .= chainIDQueryDirectoryResponse
        ]

instance FromJSON QueryDirectoryResponse where
    parseJSON (Object v) = QueryDirectoryResponse
        <$> v .: "type"
        <*> v .: "mainChain"
        <*> v .: "merkleState"
        <*> v .: "data"
        <*> v .: "chainId"

instance ToJSON MainChain where
    toJSON (MainChain heightMainChain countMainChain rootsMainChain) =
        object
        [ "height" .= heightMainChain
        , "count" .= countMainChain
        , "roots" .= rootsMainChain
        ]

instance FromJSON MainChain where
    parseJSON (Object v) = MainChain
        <$> v .: "height"
        <*> v .: "count"
        <*> v .: "roots"

instance ToJSON Data where
    toJSON (Data dataTypeData urlData keyBookData managerKeyBookData) =
        object
        [ "type" .= dataTypeData
        , "url" .= urlData
        , "keyBook" .= keyBookData
        , "managerKeyBook" .= managerKeyBookData
        ]

instance FromJSON Data where
    parseJSON (Object v) = Data
        <$> v .: "type"
        <*> v .: "url"
        <*> v .: "keyBook"
        <*> v .: "managerKeyBook"
