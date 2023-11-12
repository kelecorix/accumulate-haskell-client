{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Accumulate.RPC.V3.Types.ApiMajorBlocksRangeResponse
    ( MajorBlocksRangeResponse (..)
    , Record (..)
    , decodeTopLevel
    ) where

import           Data.Aeson
import           Data.Aeson.Types     (emptyObject)
import           Data.ByteString.Lazy (ByteString)
import           Data.HashMap.Strict  (HashMap)
import           Data.Text            (Text)

data Record = Record
    { indexRecord       :: Int
    , timeRecord        :: Text
    , recordTypeRecord  :: Text
    , minorBlocksRecord :: MajorBlocksRangeResponse
    } deriving (Show)

data MajorBlocksRangeResponse = MajorBlocksRangeResponse
    { recordsMajorBlocksRangeResponse    :: Maybe ([Record])
    , totalMajorBlocksRangeResponse      :: Int
    , startMajorBlocksRangeResponse      :: Int
    , recordTypeMajorBlocksRangeResponse :: Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe MajorBlocksRangeResponse
decodeTopLevel = decode

instance ToJSON Record where
    toJSON (Record indexRecord timeRecord recordTypeRecord minorBlocksRecord) =
        object
        [ "index" .= indexRecord
        , "time" .= timeRecord
        , "recordType" .= recordTypeRecord
        , "minorBlocks" .= minorBlocksRecord
        ]

instance FromJSON Record where
    parseJSON (Object v) = Record
        <$> v .: "index"
        <*> v .: "time"
        <*> v .: "recordType"
        <*> v .: "minorBlocks"

instance ToJSON MajorBlocksRangeResponse where
    toJSON (MajorBlocksRangeResponse recordsMajorBlocksRangeResponse totalMajorBlocksRangeResponse startMajorBlocksRangeResponse recordTypeMajorBlocksRangeResponse) =
        object
        [ "records" .= recordsMajorBlocksRangeResponse
        , "total" .= totalMajorBlocksRangeResponse
        , "start" .= startMajorBlocksRangeResponse
        , "recordType" .= recordTypeMajorBlocksRangeResponse
        ]

instance FromJSON MajorBlocksRangeResponse where
    parseJSON (Object v) = MajorBlocksRangeResponse
        <$> v .:? "records"
        <*> v .: "total"
        <*> v .: "start"
        <*> v .: "recordType"
