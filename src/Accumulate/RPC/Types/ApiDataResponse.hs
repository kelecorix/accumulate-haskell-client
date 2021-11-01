{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Accumulate.RPC.Types.ApiDataResponse
  ( APIDataResponse (..),
    Data (..),
    decodeApiData,
  )
where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)

--------------------------------------------------------------------------------

data APIDataResponse = APIDataResponse
  { apiDataResponseDataAPIDataResponse :: Maybe Data,
    sponsorAPIDataResponse :: Maybe Text,
    keyPageAPIDataResponse :: Maybe (Maybe Text),
    mdRootAPIDataResponse :: Maybe Text,
    apiDataResponseTypeAPIDataResponse :: Maybe Text
  }
  deriving (Show)

data Data = Data
  { txCountData :: Maybe Int,
    urlData :: Maybe Text,
    balanceData :: Maybe Text,
    keyBookURLData :: Maybe Text,
    tokenURLData :: Maybe Text,
    creditBalanceData :: Maybe Text,
    nonceData :: Maybe Int
  }
  deriving (Show)

decodeApiData :: ByteString -> Maybe APIDataResponse
decodeApiData = decode

instance ToJSON APIDataResponse where
  toJSON (APIDataResponse apiDataResponseDataAPIDataResponse sponsorAPIDataResponse keyPageAPIDataResponse mdRootAPIDataResponse apiDataResponseTypeAPIDataResponse) =
    object
      [ "data" .= apiDataResponseDataAPIDataResponse,
        "sponsor" .= sponsorAPIDataResponse,
        "keyPage" .= keyPageAPIDataResponse,
        "mdRoot" .= mdRootAPIDataResponse,
        "type" .= apiDataResponseTypeAPIDataResponse
      ]

instance FromJSON APIDataResponse where
  parseJSON (Object v) =
    APIDataResponse
      <$> v .:? "data"
      <*> v .:? "sponsor"
      <*> v .:? "keyPage"
      <*> v .:? "mdRoot"
      <*> v .:? "type"

instance ToJSON Data where
  toJSON (Data txCountData urlData balanceData keyBookURLData tokenURLData creditBalanceData nonceData) =
    object
      [ "txCount" .= txCountData,
        "url" .= urlData,
        "balance" .= balanceData,
        "keyBookUrl" .= keyBookURLData,
        "tokenUrl" .= tokenURLData,
        "creditBalance" .= creditBalanceData,
        "nonce" .= nonceData
      ]

instance FromJSON Data where
  parseJSON (Object v) =
    Data
      <$> v .:? "txCount"
      <*> v .:? "url"
      <*> v .:? "balance"
      <*> v .:? "keyBookUrl"
      <*> v .:? "tokenUrl"
      <*> v .:? "creditBalance"
      <*> v .:? "nonce"
