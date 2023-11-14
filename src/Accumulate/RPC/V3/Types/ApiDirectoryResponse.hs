{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Accumulate.RPC.V3.Types.ApiDirectoryResponse
    ( DirectoryResponse (..)
    , Account (..)
    , Authority (..)
    , Directory (..)
    , Record (..)
    , decodeTopLevel
    ) where

import           Data.Aeson
import           Data.Aeson.Types     (emptyObject)
import           Data.ByteString.Lazy (ByteString)
import           Data.HashMap.Strict  (HashMap)
import           Data.Text            (Text)

data DirectoryResponse = DirectoryResponse
    { directoryDirectoryResponse  :: Directory
    , accountDirectoryResponse    :: Account
    , recordTypeDirectoryResponse :: Text
    , pendingDirectoryResponse    :: Directory
    } deriving (Show)

data Account = Account
    { authoritiesAccount :: [Authority]
    , accountTypeAccount :: Text
    , urlAccount         :: Text
    } deriving (Show)

data Authority = Authority
    { urlAuthority :: Text
    } deriving (Show)

data Directory = Directory
    { recordsDirectory    :: Maybe ([Record])
    , totalDirectory      :: Int
    , startDirectory      :: Int
    , recordTypeDirectory :: Text
    } deriving (Show)

data Record = Record
    { recordTypeRecord :: Text
    , valueRecord      :: Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe DirectoryResponse
decodeTopLevel = decode

instance ToJSON DirectoryResponse where
    toJSON (DirectoryResponse directoryDirectoryResponse accountDirectoryResponse recordTypeDirectoryResponse pendingDirectoryResponse) =
        object
        [ "directory" .= directoryDirectoryResponse
        , "account" .= accountDirectoryResponse
        , "recordType" .= recordTypeDirectoryResponse
        , "pending" .= pendingDirectoryResponse
        ]

instance FromJSON DirectoryResponse where
    parseJSON (Object v) = DirectoryResponse
        <$> v .: "directory"
        <*> v .: "account"
        <*> v .: "recordType"
        <*> v .: "pending"

instance ToJSON Account where
    toJSON (Account authoritiesAccount accountTypeAccount urlAccount) =
        object
        [ "authorities" .= authoritiesAccount
        , "type" .= accountTypeAccount
        , "url" .= urlAccount
        ]

instance FromJSON Account where
    parseJSON (Object v) = Account
        <$> v .: "authorities"
        <*> v .: "type"
        <*> v .: "url"

instance ToJSON Authority where
    toJSON (Authority urlAuthority) =
        object
        [ "url" .= urlAuthority
        ]

instance FromJSON Authority where
    parseJSON (Object v) = Authority
        <$> v .: "url"

instance ToJSON Directory where
    toJSON (Directory recordsDirectory totalDirectory startDirectory recordTypeDirectory) =
        object
        [ "records" .= recordsDirectory
        , "total" .= totalDirectory
        , "start" .= startDirectory
        , "recordType" .= recordTypeDirectory
        ]

instance FromJSON Directory where
    parseJSON (Object v) = Directory
        <$> v .:? "records"
        <*> v .: "total"
        <*> v .: "start"
        <*> v .: "recordType"

instance ToJSON Record where
    toJSON (Record recordTypeRecord valueRecord) =
        object
        [ "recordType" .= recordTypeRecord
        , "value" .= valueRecord
        ]

instance FromJSON Record where
    parseJSON (Object v) = Record
        <$> v .: "recordType"
        <*> v .: "value"
