{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Accumulate.RPC.V3.Types.ApiMetricsResponse
    ( MetricsResponse (..)
    , decodeMetrics
    ) where

import           Data.Aeson
import           Data.Aeson.Types     (emptyObject)
import           Data.ByteString.Lazy (ByteString)
import           Data.HashMap.Strict  (HashMap)
import           Data.Text            (Text)

data MetricsResponse = MetricsResponse
    { tpsMetricsResponse :: Float
    } deriving (Show)

decodeMetrics :: ByteString -> Maybe MetricsResponse
decodeMetrics = decode

instance ToJSON MetricsResponse where
    toJSON (MetricsResponse tpsMetricsResponse) =
        object
        [ "tps" .= tpsMetricsResponse
        ]

instance FromJSON MetricsResponse where
    parseJSON (Object v) = MetricsResponse
        <$> v .: "tps"
