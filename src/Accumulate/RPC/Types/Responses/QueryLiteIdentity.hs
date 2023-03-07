{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Accumulate.RPC.Types.Responses.QueryLiteIdentity
    ( QueryResponseLiteIdentity (..)
    , Chain (..)
    , MainChain (..)
    , Data (..)
    , decodeTopLevel
    ) where

import           Data.Aeson
import           Data.Aeson.Types     (emptyObject)
import           Data.ByteString.Lazy (ByteString)
import           Data.HashMap.Strict  (HashMap)
import           Data.Text            (Text)

data QueryResponseLiteIdentity = QueryResponseLiteIdentity
    { merkleStateQueryResponseLiteIdentity                   :: MainChain
    , queryResponseLiteIdentityTypeQueryResponseLiteIdentity :: Text
    , chainsQueryResponseLiteIdentity                        :: [Chain]
    , chainIDQueryResponseLiteIdentity                       :: Text
    , queryResponseLiteIdentityDataQueryResponseLiteIdentity :: Data
    , mainChainQueryResponseLiteIdentity                     :: MainChain
    } deriving (Show)

data Chain = Chain
    { nameChain      :: Text
    , chainTypeChain :: Text
    , countChain     :: Int
    , rootsChain     :: [Text]
    , heightChain    :: Int
    } deriving (Show)

data MainChain = MainChain
    { countMainChain  :: Int
    , rootsMainChain  :: [Text]
    , heightMainChain :: Int
    } deriving (Show)

data Data = Data
    { dataTypeData   :: Text
    , urlData        :: Text
    , lastUsedOnData :: Int
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe QueryResponseLiteIdentity
decodeTopLevel = decode

instance ToJSON QueryResponseLiteIdentity where
    toJSON (QueryResponseLiteIdentity merkleStateQueryResponseLiteIdentity queryResponseLiteIdentityTypeQueryResponseLiteIdentity chainsQueryResponseLiteIdentity chainIDQueryResponseLiteIdentity queryResponseLiteIdentityDataQueryResponseLiteIdentity mainChainQueryResponseLiteIdentity) =
        object
        [ "merkleState" .= merkleStateQueryResponseLiteIdentity
        , "type" .= queryResponseLiteIdentityTypeQueryResponseLiteIdentity
        , "chains" .= chainsQueryResponseLiteIdentity
        , "chainId" .= chainIDQueryResponseLiteIdentity
        , "data" .= queryResponseLiteIdentityDataQueryResponseLiteIdentity
        , "mainChain" .= mainChainQueryResponseLiteIdentity
        ]

instance FromJSON QueryResponseLiteIdentity where
    parseJSON (Object v) = QueryResponseLiteIdentity
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
    toJSON (Data dataTypeData urlData lastUsedOnData) =
        object
        [ "type" .= dataTypeData
        , "url" .= urlData
        , "lastUsedOn" .= lastUsedOnData
        ]

instance FromJSON Data where
    parseJSON (Object v) = Data
        <$> v .: "type"
        <*> v .: "url"
        <*> v .: "lastUsedOn"
