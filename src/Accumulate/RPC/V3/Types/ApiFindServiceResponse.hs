{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Accumulate.RPC.V3.Types.ApiFindServiceResponse
    ( FindServiceResponse (..)
    , FindServiceResponseElement (..)
    , decodeResponse
    ) where

import           Data.Aeson
import           Data.Aeson.Types     (emptyObject)
import           Data.ByteString.Lazy (ByteString)
import           Data.HashMap.Strict  (HashMap)
import           Data.Text            (Text)

type FindServiceResponse = [FindServiceResponseElement]

data FindServiceResponseElement = FindServiceResponseElement
    { peerIDFindServiceResponseElement    :: Text
    , statusFindServiceResponseElement    :: Maybe Text
    , addressesFindServiceResponseElement :: Maybe ([Text])
    } deriving (Show)

decodeResponse :: ByteString -> Maybe FindServiceResponse
decodeResponse = decode

instance ToJSON FindServiceResponseElement where
    toJSON (FindServiceResponseElement peerIDFindServiceResponseElement addressesFindServiceResponseElement statusFindServiceResponseElement) =
        object
        [ "peerID"    .= peerIDFindServiceResponseElement
        , "status"    .= statusFindServiceResponseElement
        , "addresses" .= addressesFindServiceResponseElement
        ]

instance FromJSON FindServiceResponseElement where
    parseJSON (Object v) = FindServiceResponseElement
        <$> v .:  "peerID"
        <*> v .:? "status"
        <*> v .:? "addresses"
