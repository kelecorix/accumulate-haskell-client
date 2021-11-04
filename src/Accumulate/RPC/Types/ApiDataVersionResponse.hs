{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Accumulate.RPC.Types.ApiDataVersionResponse
    ( APIDataVersionResponse (..)
    , Data (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)

data APIDataVersionResponse = APIDataVersionResponse
    { apiDataVersionResponseDataAPIDataVersionResponse :: Data
    , sponsorAPIDataVersionResponse :: Text
    , keyPageAPIDataVersionResponse :: Maybe Text
    , apiDataVersionResponseTypeAPIDataVersionResponse :: Text
    } deriving (Show)

data Data = Data
    { versionIsKnownData :: Bool
    , versionData :: Text
    , commitData :: Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe APIDataVersionResponse
decodeTopLevel = decode

instance ToJSON APIDataVersionResponse where
    toJSON (APIDataVersionResponse apiDataVersionResponseDataAPIDataVersionResponse sponsorAPIDataVersionResponse keyPageAPIDataVersionResponse apiDataVersionResponseTypeAPIDataVersionResponse) =
        object
        [ "data" .= apiDataVersionResponseDataAPIDataVersionResponse
        , "sponsor" .= sponsorAPIDataVersionResponse
        , "keyPage" .= keyPageAPIDataVersionResponse
        , "type" .= apiDataVersionResponseTypeAPIDataVersionResponse
        ]

instance FromJSON APIDataVersionResponse where
    parseJSON (Object v) = APIDataVersionResponse
        <$> v .: "data"
        <*> v .: "sponsor"
        <*> v .: "keyPage"
        <*> v .: "type"

instance ToJSON Data where
    toJSON (Data versionIsKnownData versionData commitData) =
        object
        [ "versionIsKnown" .= versionIsKnownData
        , "version" .= versionData
        , "commit" .= commitData
        ]

instance FromJSON Data where
    parseJSON (Object v) = Data
        <$> v .: "versionIsKnown"
        <*> v .: "version"
        <*> v .: "commit"
