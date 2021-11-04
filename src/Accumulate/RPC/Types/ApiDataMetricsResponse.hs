{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Accumulate.RPC.Types.ApiDataMetricsResponse
    ( APIDataMetricsResponse (..)
    , Data (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)

data APIDataMetricsResponse = APIDataMetricsResponse
    { apiDataMetricsResponseDataAPIDataMetricsResponse :: Data
    , sponsorAPIDataMetricsResponse :: Text
    , keyPageAPIDataMetricsResponse :: Maybe Text
    , apiDataMetricsResponseTypeAPIDataMetricsResponse :: Text
    } deriving (Show)

data Data = Data
    { valueData :: Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe APIDataMetricsResponse
decodeTopLevel = decode

instance ToJSON APIDataMetricsResponse where
    toJSON (APIDataMetricsResponse apiDataMetricsResponseDataAPIDataMetricsResponse sponsorAPIDataMetricsResponse keyPageAPIDataMetricsResponse apiDataMetricsResponseTypeAPIDataMetricsResponse) =
        object
        [ "data" .= apiDataMetricsResponseDataAPIDataMetricsResponse
        , "sponsor" .= sponsorAPIDataMetricsResponse
        , "keyPage" .= keyPageAPIDataMetricsResponse
        , "type" .= apiDataMetricsResponseTypeAPIDataMetricsResponse
        ]

instance FromJSON APIDataMetricsResponse where
    parseJSON (Object v) = APIDataMetricsResponse
        <$> v .: "data"
        <*> v .: "sponsor"
        <*> v .: "keyPage"
        <*> v .: "type"

instance ToJSON Data where
    toJSON (Data valueData) =
        object
        [ "value" .= valueData
        ]

instance FromJSON Data where
    parseJSON (Object v) = Data
        <$> v .: "value"
