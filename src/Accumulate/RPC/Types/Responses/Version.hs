{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Accumulate.RPC.Types.Responses.Version
    ( VersionResponse (..)
    , Data (..)
    , decodeResponseVersion
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)

data VersionResponse = VersionResponse
    { versionResponseDataVersionResponse :: Data
    , versionResponseTypeVersionResponse :: Text
    } deriving (Show)

data Data = Data
    { commitData :: Text
    , versionData :: Text
    , versionIsKnownData :: Bool
    } deriving (Show)

decodeResponseVersion :: ByteString -> Maybe VersionResponse
decodeResponseVersion = decode

instance ToJSON VersionResponse where
    toJSON (VersionResponse versionResponseDataVersionResponse versionResponseTypeVersionResponse) =
        object
        [ "data" .= versionResponseDataVersionResponse
        , "type" .= versionResponseTypeVersionResponse
        ]

instance FromJSON VersionResponse where
    parseJSON (Object v) = VersionResponse
        <$> v .: "data"
        <*> v .: "type"

instance ToJSON Data where
    toJSON (Data commitData versionData versionIsKnownData) =
        object
        [ "commit" .= commitData
        , "version" .= versionData
        , "versionIsKnown" .= versionIsKnownData
        ]

instance FromJSON Data where
    parseJSON (Object v) = Data
        <$> v .: "commit"
        <*> v .: "version"
        <*> v .: "versionIsKnown"
