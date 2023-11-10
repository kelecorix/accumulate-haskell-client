{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Accumulate.RPC.Types.ApiNetworkDescribeResponse
    ( APINetworkDescribeResponse (..)
    , APINetworkDescribeResponseNetwork (..)
    , SubnetElement (..)
    , Node (..)
    , Values (..)
    , Globals (..)
    , FeeSchedule (..)
    , Limits (..)
    , AtorAcceptThreshold (..)
    , ValuesNetwork (..)
    , PurplePartition (..)
    , Validator (..)
    , ValidatorPartition (..)
    , Oracle (..)
    , Routing (..)
    , Override (..)
    , Route (..)
    , decodeTopLevel
    ) where

import           Data.Aeson
import           Data.Aeson.Types     (emptyObject)
import           Data.ByteString.Lazy (ByteString)
import           Data.HashMap.Strict  (HashMap)
import           Data.Text            (Text)

data APINetworkDescribeResponse = APINetworkDescribeResponse
    { networkAPINetworkDescribeResponse     :: APINetworkDescribeResponseNetwork
    , partitionIDAPINetworkDescribeResponse :: Text
    , valuesAPINetworkDescribeResponse      :: Values
    , networkTypeAPINetworkDescribeResponse :: Text
    } deriving (Show)

data APINetworkDescribeResponseNetwork = APINetworkDescribeResponseNetwork
    { subnetsAPINetworkDescribeResponseNetwork    :: [SubnetElement]
    , networkIDAPINetworkDescribeResponseNetwork  :: Text
    , partitionsAPINetworkDescribeResponseNetwork :: [SubnetElement]
    } deriving (Show)

data SubnetElement = SubnetElement
    { partitionTypeSubnetElement :: Text
    , nodesSubnetElement         :: [Node]
    , basePortSubnetElement      :: Int
    , partitionIDSubnetElement   :: Text
    } deriving (Show)

data Node = Node
    { nodeTypeNode :: Text
    , addressNode  :: Text
    } deriving (Show)

data Values = Values
    { globalsValues :: Globals
    , networkValues :: ValuesNetwork
    , routingValues :: Routing
    , oracleValues  :: Oracle
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

data ValuesNetwork = ValuesNetwork
    { partitionsValuesNetwork  :: [PurplePartition]
    , networkNameValuesNetwork :: Text
    , validatorsValuesNetwork  :: [Validator]
    } deriving (Show)

data PurplePartition = PurplePartition
    { partitionTypePurplePartition :: Text
    , partitionIDPurplePartition   :: Text
    } deriving (Show)

data Validator = Validator
    { publicKeyValidator     :: Text
    , partitionsValidator    :: [ValidatorPartition]
    , publicKeyHashValidator :: Text
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

decodeTopLevel :: ByteString -> Maybe APINetworkDescribeResponse
decodeTopLevel = decode

instance ToJSON APINetworkDescribeResponse where
    toJSON (APINetworkDescribeResponse networkAPINetworkDescribeResponse partitionIDAPINetworkDescribeResponse valuesAPINetworkDescribeResponse networkTypeAPINetworkDescribeResponse) =
        object
        [ "network" .= networkAPINetworkDescribeResponse
        , "partitionId" .= partitionIDAPINetworkDescribeResponse
        , "values" .= valuesAPINetworkDescribeResponse
        , "networkType" .= networkTypeAPINetworkDescribeResponse
        ]

instance FromJSON APINetworkDescribeResponse where
    parseJSON (Object v) = APINetworkDescribeResponse
        <$> v .: "network"
        <*> v .: "partitionId"
        <*> v .: "values"
        <*> v .: "networkType"

instance ToJSON APINetworkDescribeResponseNetwork where
    toJSON (APINetworkDescribeResponseNetwork subnetsAPINetworkDescribeResponseNetwork networkIDAPINetworkDescribeResponseNetwork partitionsAPINetworkDescribeResponseNetwork) =
        object
        [ "subnets" .= subnetsAPINetworkDescribeResponseNetwork
        , "id" .= networkIDAPINetworkDescribeResponseNetwork
        , "partitions" .= partitionsAPINetworkDescribeResponseNetwork
        ]

instance FromJSON APINetworkDescribeResponseNetwork where
    parseJSON (Object v) = APINetworkDescribeResponseNetwork
        <$> v .: "subnets"
        <*> v .: "id"
        <*> v .: "partitions"

instance ToJSON SubnetElement where
    toJSON (SubnetElement partitionTypeSubnetElement nodesSubnetElement basePortSubnetElement partitionIDSubnetElement) =
        object
        [ "type" .= partitionTypeSubnetElement
        , "nodes" .= nodesSubnetElement
        , "basePort" .= basePortSubnetElement
        , "id" .= partitionIDSubnetElement
        ]

instance FromJSON SubnetElement where
    parseJSON (Object v) = SubnetElement
        <$> v .: "type"
        <*> v .: "nodes"
        <*> v .: "basePort"
        <*> v .: "id"

instance ToJSON Node where
    toJSON (Node nodeTypeNode addressNode) =
        object
        [ "type" .= nodeTypeNode
        , "address" .= addressNode
        ]

instance FromJSON Node where
    parseJSON (Object v) = Node
        <$> v .: "type"
        <*> v .: "address"

instance ToJSON Values where
    toJSON (Values globalsValues networkValues routingValues oracleValues) =
        object
        [ "globals" .= globalsValues
        , "network" .= networkValues
        , "routing" .= routingValues
        , "oracle" .= oracleValues
        ]

instance FromJSON Values where
    parseJSON (Object v) = Values
        <$> v .: "globals"
        <*> v .: "network"
        <*> v .: "routing"
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

instance ToJSON ValuesNetwork where
    toJSON (ValuesNetwork partitionsValuesNetwork networkNameValuesNetwork validatorsValuesNetwork) =
        object
        [ "partitions" .= partitionsValuesNetwork
        , "networkName" .= networkNameValuesNetwork
        , "validators" .= validatorsValuesNetwork
        ]

instance FromJSON ValuesNetwork where
    parseJSON (Object v) = ValuesNetwork
        <$> v .: "partitions"
        <*> v .: "networkName"
        <*> v .: "validators"

instance ToJSON PurplePartition where
    toJSON (PurplePartition partitionTypePurplePartition partitionIDPurplePartition) =
        object
        [ "type" .= partitionTypePurplePartition
        , "id" .= partitionIDPurplePartition
        ]

instance FromJSON PurplePartition where
    parseJSON (Object v) = PurplePartition
        <$> v .: "type"
        <*> v .: "id"

instance ToJSON Validator where
    toJSON (Validator publicKeyValidator partitionsValidator publicKeyHashValidator) =
        object
        [ "publicKey" .= publicKeyValidator
        , "partitions" .= partitionsValidator
        , "publicKeyHash" .= publicKeyHashValidator
        ]

instance FromJSON Validator where
    parseJSON (Object v) = Validator
        <$> v .: "publicKey"
        <*> v .: "partitions"
        <*> v .: "publicKeyHash"

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
