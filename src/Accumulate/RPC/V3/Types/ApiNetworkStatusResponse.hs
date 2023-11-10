{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Accumulate.RPC.V3.Types.ApiNetworkStatusResponse
    ( NetworkStatusResponse (..)
    , Globals (..)
    , FeeSchedule (..)
    , Limits (..)
    , AtorAcceptThreshold (..)
    , Network (..)
    , NetworkPartition (..)
    , Validator (..)
    , ValidatorPartition (..)
    , Oracle (..)
    , Routing (..)
    , Override (..)
    , Route (..)
    , decodeNetworkStatus
    ) where

import           Data.Aeson
import           Data.Aeson.Types     (emptyObject)
import           Data.ByteString.Lazy (ByteString)
import           Data.HashMap.Strict  (HashMap)
import           Data.Text            (Text)

data NetworkStatusResponse = NetworkStatusResponse
    { globalsNetworkStatusResponse          :: Globals
    , majorBlockHeightNetworkStatusResponse :: Int
    , networkNetworkStatusResponse          :: Network
    , directoryHeightNetworkStatusResponse  :: Int
    , routingNetworkStatusResponse          :: Routing
    , executorVersionNetworkStatusResponse  :: Text
    , oracleNetworkStatusResponse           :: Oracle
    } deriving (Show)

data Globals = Globals
    { validatorAcceptThresholdGlobals :: AtorAcceptThreshold
    , operatorAcceptThresholdGlobals  :: AtorAcceptThreshold
    , limitsGlobals                   :: Limits
    , majorBlockScheduleGlobals       :: Text
    , feeScheduleGlobals              :: FeeSchedule
    } deriving (Show)

data FeeSchedule = FeeSchedule
    { createIdentitySlidingFeeSchedule :: [Int]
    } deriving (Show)

data Limits = Limits
    { accountAuthoritiesLimits :: Int
    , dataEntryPartsLimits     :: Int
    , pageEntriesLimits        :: Int
    , bookPagesLimits          :: Int
    , identityAccountsLimits   :: Int
    } deriving (Show)

data AtorAcceptThreshold = AtorAcceptThreshold
    { numeratorAtorAcceptThreshold   :: Int
    , denominatorAtorAcceptThreshold :: Int
    } deriving (Show)

data Network = Network
    { partitionsNetwork  :: [NetworkPartition]
    , networkNameNetwork :: Text
    , validatorsNetwork  :: [Validator]
    , versionNetwork     :: Int
    } deriving (Show)

data NetworkPartition = NetworkPartition
    { partitionTypeNetworkPartition :: Text
    , partitionIDNetworkPartition   :: Text
    } deriving (Show)

data Validator = Validator
    { publicKeyValidator     :: Text
    , partitionsValidator    :: Maybe ([ValidatorPartition])
    , publicKeyHashValidator :: Text
    , operatorValidator      :: Text
    } deriving (Show)

data ValidatorPartition = ValidatorPartition
    { activeValidatorPartition      :: Bool
    , partitionIDValidatorPartition :: Text
    } deriving (Show)

data Oracle = Oracle
    { priceOracle :: Int
    } deriving (Show)

data Routing = Routing
    { routesRouting    :: [Route]
    , overridesRouting :: [Override]
    } deriving (Show)

data Override = Override
    { accountOverride   :: Text
    , partitionOverride :: Text
    } deriving (Show)

data Route = Route
    { lengthRoute    :: Int
    , partitionRoute :: Text
    , valueRoute     :: Maybe Int
    } deriving (Show)

decodeNetworkStatus :: ByteString -> Maybe NetworkStatusResponse
decodeNetworkStatus = decode

instance ToJSON NetworkStatusResponse where
    toJSON (NetworkStatusResponse globalsNetworkStatusResponse majorBlockHeightNetworkStatusResponse networkNetworkStatusResponse directoryHeightNetworkStatusResponse routingNetworkStatusResponse executorVersionNetworkStatusResponse oracleNetworkStatusResponse) =
        object
        [ "globals" .= globalsNetworkStatusResponse
        , "majorBlockHeight" .= majorBlockHeightNetworkStatusResponse
        , "network" .= networkNetworkStatusResponse
        , "directoryHeight" .= directoryHeightNetworkStatusResponse
        , "routing" .= routingNetworkStatusResponse
        , "executorVersion" .= executorVersionNetworkStatusResponse
        , "oracle" .= oracleNetworkStatusResponse
        ]

instance FromJSON NetworkStatusResponse where
    parseJSON (Object v) = NetworkStatusResponse
        <$> v .: "globals"
        <*> v .: "majorBlockHeight"
        <*> v .: "network"
        <*> v .: "directoryHeight"
        <*> v .: "routing"
        <*> v .: "executorVersion"
        <*> v .: "oracle"

instance ToJSON Globals where
    toJSON (Globals validatorAcceptThresholdGlobals operatorAcceptThresholdGlobals limitsGlobals majorBlockScheduleGlobals feeScheduleGlobals) =
        object
        [ "validatorAcceptThreshold" .= validatorAcceptThresholdGlobals
        , "operatorAcceptThreshold" .= operatorAcceptThresholdGlobals
        , "limits" .= limitsGlobals
        , "majorBlockSchedule" .= majorBlockScheduleGlobals
        , "feeSchedule" .= feeScheduleGlobals
        ]

instance FromJSON Globals where
    parseJSON (Object v) = Globals
        <$> v .: "validatorAcceptThreshold"
        <*> v .: "operatorAcceptThreshold"
        <*> v .: "limits"
        <*> v .: "majorBlockSchedule"
        <*> v .: "feeSchedule"

instance ToJSON FeeSchedule where
    toJSON (FeeSchedule createIdentitySlidingFeeSchedule) =
        object
        [ "createIdentitySliding" .= createIdentitySlidingFeeSchedule
        ]

instance FromJSON FeeSchedule where
    parseJSON (Object v) = FeeSchedule
        <$> v .: "createIdentitySliding"

instance ToJSON Limits where
    toJSON (Limits accountAuthoritiesLimits dataEntryPartsLimits pageEntriesLimits bookPagesLimits identityAccountsLimits) =
        object
        [ "accountAuthorities" .= accountAuthoritiesLimits
        , "dataEntryParts" .= dataEntryPartsLimits
        , "pageEntries" .= pageEntriesLimits
        , "bookPages" .= bookPagesLimits
        , "identityAccounts" .= identityAccountsLimits
        ]

instance FromJSON Limits where
    parseJSON (Object v) = Limits
        <$> v .: "accountAuthorities"
        <*> v .: "dataEntryParts"
        <*> v .: "pageEntries"
        <*> v .: "bookPages"
        <*> v .: "identityAccounts"

instance ToJSON AtorAcceptThreshold where
    toJSON (AtorAcceptThreshold numeratorAtorAcceptThreshold denominatorAtorAcceptThreshold) =
        object
        [ "numerator" .= numeratorAtorAcceptThreshold
        , "denominator" .= denominatorAtorAcceptThreshold
        ]

instance FromJSON AtorAcceptThreshold where
    parseJSON (Object v) = AtorAcceptThreshold
        <$> v .: "numerator"
        <*> v .: "denominator"

instance ToJSON Network where
    toJSON (Network partitionsNetwork networkNameNetwork validatorsNetwork versionNetwork) =
        object
        [ "partitions" .= partitionsNetwork
        , "networkName" .= networkNameNetwork
        , "validators" .= validatorsNetwork
        , "version" .= versionNetwork
        ]

instance FromJSON Network where
    parseJSON (Object v) = Network
        <$> v .: "partitions"
        <*> v .: "networkName"
        <*> v .: "validators"
        <*> v .: "version"

instance ToJSON NetworkPartition where
    toJSON (NetworkPartition partitionTypeNetworkPartition partitionIDNetworkPartition) =
        object
        [ "type" .= partitionTypeNetworkPartition
        , "id" .= partitionIDNetworkPartition
        ]

instance FromJSON NetworkPartition where
    parseJSON (Object v) = NetworkPartition
        <$> v .: "type"
        <*> v .: "id"

instance ToJSON Validator where
    toJSON (Validator publicKeyValidator partitionsValidator publicKeyHashValidator operatorValidator) =
        object
        [ "publicKey" .= publicKeyValidator
        , "partitions" .= partitionsValidator
        , "publicKeyHash" .= publicKeyHashValidator
        , "operator" .= operatorValidator
        ]

instance FromJSON Validator where
    parseJSON (Object v) = Validator
        <$> v .: "publicKey"
        <*> v .:? "partitions"
        <*> v .: "publicKeyHash"
        <*> v .: "operator"

instance ToJSON ValidatorPartition where
    toJSON (ValidatorPartition activeValidatorPartition partitionIDValidatorPartition) =
        object
        [ "active" .= activeValidatorPartition
        , "id" .= partitionIDValidatorPartition
        ]

instance FromJSON ValidatorPartition where
    parseJSON (Object v) = ValidatorPartition
        <$> v .: "active"
        <*> v .: "id"

instance ToJSON Oracle where
    toJSON (Oracle priceOracle) =
        object
        [ "price" .= priceOracle
        ]

instance FromJSON Oracle where
    parseJSON (Object v) = Oracle
        <$> v .: "price"

instance ToJSON Routing where
    toJSON (Routing routesRouting overridesRouting) =
        object
        [ "routes" .= routesRouting
        , "overrides" .= overridesRouting
        ]

instance FromJSON Routing where
    parseJSON (Object v) = Routing
        <$> v .: "routes"
        <*> v .: "overrides"

instance ToJSON Override where
    toJSON (Override accountOverride partitionOverride) =
        object
        [ "account" .= accountOverride
        , "partition" .= partitionOverride
        ]

instance FromJSON Override where
    parseJSON (Object v) = Override
        <$> v .: "account"
        <*> v .: "partition"

instance ToJSON Route where
    toJSON (Route lengthRoute partitionRoute valueRoute) =
        object
        [ "length" .= lengthRoute
        , "partition" .= partitionRoute
        , "value" .= valueRoute
        ]

instance FromJSON Route where
    parseJSON (Object v) = Route
        <$> v .: "length"
        <*> v .: "partition"
        <*> v .:? "value"
