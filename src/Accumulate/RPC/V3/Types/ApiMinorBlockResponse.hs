{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Accumulate.RPC.V3.Types.ApiMinorBlockResponse
    ( MinorBlockResponse (..)
    , ValueClass (..)
    , Record (..)
    , Entries (..)
    , ValueMessage (..)
    , Transaction (..)
    , ResultClass (..)
    , Header (..)
    , AccountClass (..)
    , RecordMessage (..)
    , AccountUnion (..)
    , decodeTopLevel
    ) where

import           Data.Aeson
import           Data.Aeson.Types     (emptyObject)
import           Data.ByteString.Lazy (ByteString)
import           Data.HashMap.Strict  (HashMap)
import           Data.Text            (Text)

data MinorBlockResponse = MinorBlockResponse
    { indexMinorBlockResponse      :: Int
    , timeMinorBlockResponse       :: Text
    , recordTypeMinorBlockResponse :: Text
    , sourceMinorBlockResponse     :: Text
    , entriesMinorBlockResponse    :: Entries
    } deriving (Show)

data ValueClass = ValueClass
    { producedValueClass   :: Entries
    , messageValueClass    :: ValueMessage
    , sequenceValueClass   :: ResultClass
    , signaturesValueClass :: Entries
    , statusValueClass     :: Text
    , recordTypeValueClass :: Text
    , receivedValueClass   :: Int
    , valueIDValueClass    :: Text
    , statusNoValueClass   :: Int
    , resultValueClass     :: ResultClass
    , causeValueClass      :: Entries
    } deriving (Show)

data Record = Record
    { nameRecord       :: Maybe Text
    , purpleTypeRecord :: Maybe Text
    , indexRecord      :: Maybe Int
    , accountRecord    :: Maybe AccountUnion
    , recordTypeRecord :: Text
    , entryRecord      :: Maybe Text
    , valueRecord      :: Maybe ValueClass
    , signaturesRecord :: Maybe Entries
    , messageRecord    :: Maybe RecordMessage
    } deriving (Show)

data Entries = Entries
    { recordsEntries    :: Maybe ([Record])
    , totalEntries      :: Int
    , startEntries      :: Int
    , recordTypeEntries :: Text
    } deriving (Show)

data ValueMessage = ValueMessage
    { messageTypeValueMessage :: Text
    , transactionValueMessage :: Transaction
    } deriving (Show)

data Transaction = Transaction
    { headerTransaction :: Header
    , bodyTransaction   :: ResultClass
    } deriving (Show)

data ResultClass = ResultClass
    { resultTypeResultClass :: Text
    } deriving (Show)

data Header = Header
    { principalHeader :: Text
    } deriving (Show)

data AccountUnion
    = AccountClassInAccountUnion AccountClass
    | StringInAccountUnion Text
    deriving (Show)

data AccountClass = AccountClass
    { accountTypeAccountClass :: Text
    , urlAccountClass         :: Text
    } deriving (Show)

data RecordMessage = RecordMessage
    { messageTypeRecordMessage :: Text
    , payerRecordMessage       :: Text
    , initiatorRecordMessage   :: Bool
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe MinorBlockResponse
decodeTopLevel = decode

instance ToJSON MinorBlockResponse where
    toJSON (MinorBlockResponse indexMinorBlockResponse timeMinorBlockResponse recordTypeMinorBlockResponse sourceMinorBlockResponse entriesMinorBlockResponse) =
        object
        [ "index" .= indexMinorBlockResponse
        , "time" .= timeMinorBlockResponse
        , "recordType" .= recordTypeMinorBlockResponse
        , "source" .= sourceMinorBlockResponse
        , "entries" .= entriesMinorBlockResponse
        ]

instance FromJSON MinorBlockResponse where
    parseJSON (Object v) = MinorBlockResponse
        <$> v .: "index"
        <*> v .: "time"
        <*> v .: "recordType"
        <*> v .: "source"
        <*> v .: "entries"

instance ToJSON ValueClass where
    toJSON (ValueClass producedValueClass messageValueClass sequenceValueClass signaturesValueClass statusValueClass recordTypeValueClass receivedValueClass valueIDValueClass statusNoValueClass resultValueClass causeValueClass) =
        object
        [ "produced" .= producedValueClass
        , "message" .= messageValueClass
        , "sequence" .= sequenceValueClass
        , "signatures" .= signaturesValueClass
        , "status" .= statusValueClass
        , "recordType" .= recordTypeValueClass
        , "received" .= receivedValueClass
        , "id" .= valueIDValueClass
        , "statusNo" .= statusNoValueClass
        , "result" .= resultValueClass
        , "cause" .= causeValueClass
        ]

instance FromJSON ValueClass where
    parseJSON (Object v) = ValueClass
        <$> v .: "produced"
        <*> v .: "message"
        <*> v .: "sequence"
        <*> v .: "signatures"
        <*> v .: "status"
        <*> v .: "recordType"
        <*> v .: "received"
        <*> v .: "id"
        <*> v .: "statusNo"
        <*> v .: "result"
        <*> v .: "cause"

instance ToJSON Record where
    toJSON (Record nameRecord purpleTypeRecord indexRecord accountRecord recordTypeRecord entryRecord valueRecord signaturesRecord messageRecord) =
        object
        [ "name" .= nameRecord
        , "type" .= purpleTypeRecord
        , "index" .= indexRecord
        , "account" .= accountRecord
        , "recordType" .= recordTypeRecord
        , "entry" .= entryRecord
        , "value" .= valueRecord
        , "signatures" .= signaturesRecord
        , "message" .= messageRecord
        ]

instance FromJSON Record where
    parseJSON (Object v) = Record
        <$> v .:? "name"
        <*> v .:? "type"
        <*> v .:? "index"
        <*> v .:? "account"
        <*> v .: "recordType"
        <*> v .:? "entry"
        <*> v .:? "value"
        <*> v .:? "signatures"
        <*> v .:? "message"

instance ToJSON Entries where
    toJSON (Entries recordsEntries totalEntries startEntries recordTypeEntries) =
        object
        [ "records" .= recordsEntries
        , "total" .= totalEntries
        , "start" .= startEntries
        , "recordType" .= recordTypeEntries
        ]

instance FromJSON Entries where
    parseJSON (Object v) = Entries
        <$> v .:? "records"
        <*> v .: "total"
        <*> v .: "start"
        <*> v .: "recordType"

instance ToJSON ValueMessage where
    toJSON (ValueMessage messageTypeValueMessage transactionValueMessage) =
        object
        [ "type" .= messageTypeValueMessage
        , "transaction" .= transactionValueMessage
        ]

instance FromJSON ValueMessage where
    parseJSON (Object v) = ValueMessage
        <$> v .: "type"
        <*> v .: "transaction"

instance ToJSON Transaction where
    toJSON (Transaction headerTransaction bodyTransaction) =
        object
        [ "header" .= headerTransaction
        , "body" .= bodyTransaction
        ]

instance FromJSON Transaction where
    parseJSON (Object v) = Transaction
        <$> v .: "header"
        <*> v .: "body"

instance ToJSON ResultClass where
    toJSON (ResultClass resultTypeResultClass) =
        object
        [ "type" .= resultTypeResultClass
        ]

instance FromJSON ResultClass where
    parseJSON (Object v) = ResultClass
        <$> v .: "type"

instance ToJSON Header where
    toJSON (Header principalHeader) =
        object
        [ "principal" .= principalHeader
        ]

instance FromJSON Header where
    parseJSON (Object v) = Header
        <$> v .: "principal"

instance ToJSON AccountUnion where
    toJSON (AccountClassInAccountUnion x) = toJSON x
    toJSON (StringInAccountUnion x)       = toJSON x

instance FromJSON AccountUnion where
    parseJSON xs@(Object _) = (fmap AccountClassInAccountUnion . parseJSON) xs
    parseJSON xs@(String _) = (fmap StringInAccountUnion . parseJSON) xs

instance ToJSON AccountClass where
    toJSON (AccountClass accountTypeAccountClass urlAccountClass) =
        object
        [ "type" .= accountTypeAccountClass
        , "url" .= urlAccountClass
        ]

instance FromJSON AccountClass where
    parseJSON (Object v) = AccountClass
        <$> v .: "type"
        <*> v .: "url"

instance ToJSON RecordMessage where
    toJSON (RecordMessage messageTypeRecordMessage payerRecordMessage initiatorRecordMessage) =
        object
        [ "type" .= messageTypeRecordMessage
        , "payer" .= payerRecordMessage
        , "initiator" .= initiatorRecordMessage
        ]

instance FromJSON RecordMessage where
    parseJSON (Object v) = RecordMessage
        <$> v .: "type"
        <*> v .: "payer"
        <*> v .: "initiator"
