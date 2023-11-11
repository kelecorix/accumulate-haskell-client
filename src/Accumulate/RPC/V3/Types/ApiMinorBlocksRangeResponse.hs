{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Accumulate.RPC.V3.Types.ApiMinorBlocksRangeResponse
    ( MinorBlocksRangeResponse (..)
    , ValueClass (..)
    , EntriesRecord (..)
    , Entries (..)
    , MinorBlocksRangeResponseRecord (..)
    , ValueMessage (..)
    , Transaction (..)
    , Body (..)
    , Receipt (..)
    , Anchor (..)
    , RootChainReceipt (..)
    , Entry (..)
    , Header (..)
    , ResultClass (..)
    , Sequence (..)
    , Account (..)
    , RecordMessage (..)
    , Signature (..)
    , decodeTopLevel
    ) where

import           Data.Aeson
import           Data.Aeson.Types     (emptyObject)
import           Data.ByteString.Lazy (ByteString)
import           Data.HashMap.Strict  (HashMap)
import           Data.Text            (Text)

data ValueClass = ValueClass
    { producedValueClass   :: MinorBlocksRangeResponse
    , messageValueClass    :: ValueMessage
    , sequenceValueClass   :: Sequence
    , signaturesValueClass :: MinorBlocksRangeResponse
    , statusValueClass     :: Text
    , recordTypeValueClass :: Text
    , receivedValueClass   :: Maybe Int
    , valueIDValueClass    :: Text
    , statusNoValueClass   :: Int
    , resultValueClass     :: ResultClass
    , causeValueClass      :: MinorBlocksRangeResponse
    } deriving (Show)

data EntriesRecord = EntriesRecord
    { nameEntriesRecord       :: Text
    , purpleTypeEntriesRecord :: Text
    , indexEntriesRecord      :: Int
    , accountEntriesRecord    :: Text
    , recordTypeEntriesRecord :: Text
    , entryEntriesRecord      :: Text
    , valueEntriesRecord      :: Maybe ValueClass
    } deriving (Show)

data Entries = Entries
    { recordsEntries    :: [EntriesRecord]
    , totalEntries      :: Int
    , startEntries      :: Int
    , recordTypeEntries :: Text
    } deriving (Show)

data MinorBlocksRangeResponseRecord = MinorBlocksRangeResponseRecord
    { indexMinorBlocksRangeResponseRecord      :: Maybe Int
    , timeMinorBlocksRangeResponseRecord       :: Maybe Text
    , recordTypeMinorBlocksRangeResponseRecord :: Text
    , sourceMinorBlocksRangeResponseRecord     :: Maybe Text
    , entriesMinorBlocksRangeResponseRecord    :: Maybe Entries
    , signaturesMinorBlocksRangeResponseRecord :: Maybe MinorBlocksRangeResponse
    , accountMinorBlocksRangeResponseRecord    :: Maybe Account
    , messageMinorBlocksRangeResponseRecord    :: Maybe RecordMessage
    , recordIDMinorBlocksRangeResponseRecord   :: Maybe Text
    } deriving (Show)

data MinorBlocksRangeResponse = MinorBlocksRangeResponse
    { recordsMinorBlocksRangeResponse :: Maybe ([MinorBlocksRangeResponseRecord])
    , totalMinorBlocksRangeResponse :: Int
    , startMinorBlocksRangeResponse :: Int
    , recordTypeMinorBlocksRangeResponse :: Text
    } deriving (Show)

data ValueMessage = ValueMessage
    { messageTypeValueMessage :: Text
    , transactionValueMessage :: Transaction
    } deriving (Show)

data Transaction = Transaction
    { headerTransaction :: Header
    , bodyTransaction   :: Body
    } deriving (Show)

data Body = Body
    { bodyTypeBody           :: Text
    , sourceBody             :: Maybe Text
    , stateTreeAnchorBody    :: Maybe Text
    , minorBlockIndexBody    :: Maybe Int
    , rootChainIndexBody     :: Maybe Int
    , rootChainAnchorBody    :: Maybe Text
    , makeMajorBlockTimeBody :: Maybe Text
    , receiptsBody           :: Maybe ([Receipt])
    } deriving (Show)

data Receipt = Receipt
    { anchorReceipt           :: Anchor
    , rootChainReceiptReceipt :: RootChainReceipt
    } deriving (Show)

data Anchor = Anchor
    { sourceAnchor          :: Text
    , stateTreeAnchorAnchor :: Text
    , minorBlockIndexAnchor :: Int
    , rootChainIndexAnchor  :: Int
    , rootChainAnchorAnchor :: Text
    } deriving (Show)

data RootChainReceipt = RootChainReceipt
    { startRootChainReceipt   :: Text
    , endRootChainReceipt     :: Text
    , anchorRootChainReceipt  :: Text
    , entriesRootChainReceipt :: [Entry]
    } deriving (Show)

data Entry = Entry
    { hashEntry  :: Text
    , rightEntry :: Maybe Bool
    } deriving (Show)

data Header = Header
    { principalHeader :: Maybe Text
    , initiatorHeader :: Maybe Text
    } deriving (Show)

data ResultClass = ResultClass
    { resultTypeResultClass :: Text
    } deriving (Show)

data Sequence = Sequence
    { sequenceTypeSequence :: Text
    , numberSequence       :: Maybe Int
    , sourceSequence       :: Maybe Text
    , destinationSequence  :: Maybe Text
    } deriving (Show)

data Account = Account
    { accountTypeAccount :: Text
    , urlAccount         :: Text
    } deriving (Show)

data RecordMessage = RecordMessage
    { messageTypeRecordMessage :: Text
    , payerRecordMessage       :: Maybe Text
    , initiatorRecordMessage   :: Maybe Bool
    , signatureRecordMessage   :: Maybe Signature
    , txIDRecordMessage        :: Maybe Text
    } deriving (Show)

data Signature = Signature
    { signatureTypeSignature      :: Text
    , publicKeySignature          :: Maybe Text
    , destinationNetworkSignature :: Maybe Text
    , sourceNetworkSignature      :: Maybe Text
    , sequenceNumberSignature     :: Maybe Int
    , transactionHashSignature    :: Maybe Text
    , timestampSignature          :: Maybe Int
    , signatureSignature          :: Maybe Text
    , signerSignature             :: Maybe Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe MinorBlocksRangeResponse
decodeTopLevel = decode

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
        <*> v .:? "received"
        <*> v .: "id"
        <*> v .: "statusNo"
        <*> v .: "result"
        <*> v .: "cause"

instance ToJSON EntriesRecord where
    toJSON (EntriesRecord nameEntriesRecord purpleTypeEntriesRecord indexEntriesRecord accountEntriesRecord recordTypeEntriesRecord entryEntriesRecord valueEntriesRecord) =
        object
        [ "name" .= nameEntriesRecord
        , "type" .= purpleTypeEntriesRecord
        , "index" .= indexEntriesRecord
        , "account" .= accountEntriesRecord
        , "recordType" .= recordTypeEntriesRecord
        , "entry" .= entryEntriesRecord
        , "value" .= valueEntriesRecord
        ]

instance FromJSON EntriesRecord where
    parseJSON (Object v) = EntriesRecord
        <$> v .: "name"
        <*> v .: "type"
        <*> v .: "index"
        <*> v .: "account"
        <*> v .: "recordType"
        <*> v .: "entry"
        <*> v .:? "value"

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
        <$> v .: "records"
        <*> v .: "total"
        <*> v .: "start"
        <*> v .: "recordType"

instance ToJSON MinorBlocksRangeResponseRecord where
    toJSON (MinorBlocksRangeResponseRecord indexMinorBlocksRangeResponseRecord timeMinorBlocksRangeResponseRecord recordTypeMinorBlocksRangeResponseRecord sourceMinorBlocksRangeResponseRecord entriesMinorBlocksRangeResponseRecord signaturesMinorBlocksRangeResponseRecord accountMinorBlocksRangeResponseRecord messageMinorBlocksRangeResponseRecord recordIDMinorBlocksRangeResponseRecord) =
        object
        [ "index" .= indexMinorBlocksRangeResponseRecord
        , "time" .= timeMinorBlocksRangeResponseRecord
        , "recordType" .= recordTypeMinorBlocksRangeResponseRecord
        , "source" .= sourceMinorBlocksRangeResponseRecord
        , "entries" .= entriesMinorBlocksRangeResponseRecord
        , "signatures" .= signaturesMinorBlocksRangeResponseRecord
        , "account" .= accountMinorBlocksRangeResponseRecord
        , "message" .= messageMinorBlocksRangeResponseRecord
        , "id" .= recordIDMinorBlocksRangeResponseRecord
        ]

instance FromJSON MinorBlocksRangeResponseRecord where
    parseJSON (Object v) = MinorBlocksRangeResponseRecord
        <$> v .:? "index"
        <*> v .:? "time"
        <*> v .: "recordType"
        <*> v .:? "source"
        <*> v .:? "entries"
        <*> v .:? "signatures"
        <*> v .:? "account"
        <*> v .:? "message"
        <*> v .:? "id"

instance ToJSON MinorBlocksRangeResponse where
    toJSON (MinorBlocksRangeResponse recordsMinorBlocksRangeResponse totalMinorBlocksRangeResponse startMinorBlocksRangeResponse recordTypeMinorBlocksRangeResponse) =
        object
        [ "records" .= recordsMinorBlocksRangeResponse
        , "total" .= totalMinorBlocksRangeResponse
        , "start" .= startMinorBlocksRangeResponse
        , "recordType" .= recordTypeMinorBlocksRangeResponse
        ]

instance FromJSON MinorBlocksRangeResponse where
    parseJSON (Object v) = MinorBlocksRangeResponse
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

instance ToJSON Body where
    toJSON (Body bodyTypeBody sourceBody stateTreeAnchorBody minorBlockIndexBody rootChainIndexBody rootChainAnchorBody makeMajorBlockTimeBody receiptsBody) =
        object
        [ "type" .= bodyTypeBody
        , "source" .= sourceBody
        , "stateTreeAnchor" .= stateTreeAnchorBody
        , "minorBlockIndex" .= minorBlockIndexBody
        , "rootChainIndex" .= rootChainIndexBody
        , "rootChainAnchor" .= rootChainAnchorBody
        , "makeMajorBlockTime" .= makeMajorBlockTimeBody
        , "receipts" .= receiptsBody
        ]

instance FromJSON Body where
    parseJSON (Object v) = Body
        <$> v .: "type"
        <*> v .:? "source"
        <*> v .:? "stateTreeAnchor"
        <*> v .:? "minorBlockIndex"
        <*> v .:? "rootChainIndex"
        <*> v .:? "rootChainAnchor"
        <*> v .:? "makeMajorBlockTime"
        <*> v .:? "receipts"

instance ToJSON Receipt where
    toJSON (Receipt anchorReceipt rootChainReceiptReceipt) =
        object
        [ "anchor" .= anchorReceipt
        , "rootChainReceipt" .= rootChainReceiptReceipt
        ]

instance FromJSON Receipt where
    parseJSON (Object v) = Receipt
        <$> v .: "anchor"
        <*> v .: "rootChainReceipt"

instance ToJSON Anchor where
    toJSON (Anchor sourceAnchor stateTreeAnchorAnchor minorBlockIndexAnchor rootChainIndexAnchor rootChainAnchorAnchor) =
        object
        [ "source" .= sourceAnchor
        , "stateTreeAnchor" .= stateTreeAnchorAnchor
        , "minorBlockIndex" .= minorBlockIndexAnchor
        , "rootChainIndex" .= rootChainIndexAnchor
        , "rootChainAnchor" .= rootChainAnchorAnchor
        ]

instance FromJSON Anchor where
    parseJSON (Object v) = Anchor
        <$> v .: "source"
        <*> v .: "stateTreeAnchor"
        <*> v .: "minorBlockIndex"
        <*> v .: "rootChainIndex"
        <*> v .: "rootChainAnchor"

instance ToJSON RootChainReceipt where
    toJSON (RootChainReceipt startRootChainReceipt endRootChainReceipt anchorRootChainReceipt entriesRootChainReceipt) =
        object
        [ "start" .= startRootChainReceipt
        , "end" .= endRootChainReceipt
        , "anchor" .= anchorRootChainReceipt
        , "entries" .= entriesRootChainReceipt
        ]

instance FromJSON RootChainReceipt where
    parseJSON (Object v) = RootChainReceipt
        <$> v .: "start"
        <*> v .: "end"
        <*> v .: "anchor"
        <*> v .: "entries"

instance ToJSON Entry where
    toJSON (Entry hashEntry rightEntry) =
        object
        [ "hash" .= hashEntry
        , "right" .= rightEntry
        ]

instance FromJSON Entry where
    parseJSON (Object v) = Entry
        <$> v .: "hash"
        <*> v .:? "right"

instance ToJSON Header where
    toJSON (Header principalHeader initiatorHeader) =
        object
        [ "principal" .= principalHeader
        , "initiator" .= initiatorHeader
        ]

instance FromJSON Header where
    parseJSON (Object v) = Header
        <$> v .:? "principal"
        <*> v .:? "initiator"

instance ToJSON ResultClass where
    toJSON (ResultClass resultTypeResultClass) =
        object
        [ "type" .= resultTypeResultClass
        ]

instance FromJSON ResultClass where
    parseJSON (Object v) = ResultClass
        <$> v .: "type"

instance ToJSON Sequence where
    toJSON (Sequence sequenceTypeSequence numberSequence sourceSequence destinationSequence) =
        object
        [ "type" .= sequenceTypeSequence
        , "number" .= numberSequence
        , "source" .= sourceSequence
        , "destination" .= destinationSequence
        ]

instance FromJSON Sequence where
    parseJSON (Object v) = Sequence
        <$> v .: "type"
        <*> v .:? "number"
        <*> v .:? "source"
        <*> v .:? "destination"

instance ToJSON Account where
    toJSON (Account accountTypeAccount urlAccount) =
        object
        [ "type" .= accountTypeAccount
        , "url" .= urlAccount
        ]

instance FromJSON Account where
    parseJSON (Object v) = Account
        <$> v .: "type"
        <*> v .: "url"

instance ToJSON RecordMessage where
    toJSON (RecordMessage messageTypeRecordMessage payerRecordMessage initiatorRecordMessage signatureRecordMessage txIDRecordMessage) =
        object
        [ "type" .= messageTypeRecordMessage
        , "payer" .= payerRecordMessage
        , "initiator" .= initiatorRecordMessage
        , "signature" .= signatureRecordMessage
        , "txID" .= txIDRecordMessage
        ]

instance FromJSON RecordMessage where
    parseJSON (Object v) = RecordMessage
        <$> v .: "type"
        <*> v .:? "payer"
        <*> v .:? "initiator"
        <*> v .:? "signature"
        <*> v .:? "txID"

instance ToJSON Signature where
    toJSON (Signature signatureTypeSignature publicKeySignature destinationNetworkSignature sourceNetworkSignature sequenceNumberSignature transactionHashSignature timestampSignature signatureSignature signerSignature) =
        object
        [ "type" .= signatureTypeSignature
        , "publicKey" .= publicKeySignature
        , "destinationNetwork" .= destinationNetworkSignature
        , "sourceNetwork" .= sourceNetworkSignature
        , "sequenceNumber" .= sequenceNumberSignature
        , "transactionHash" .= transactionHashSignature
        , "timestamp" .= timestampSignature
        , "signature" .= signatureSignature
        , "signer" .= signerSignature
        ]

instance FromJSON Signature where
    parseJSON (Object v) = Signature
        <$> v .: "type"
        <*> v .:? "publicKey"
        <*> v .:? "destinationNetwork"
        <*> v .:? "sourceNetwork"
        <*> v .:? "sequenceNumber"
        <*> v .:? "transactionHash"
        <*> v .:? "timestamp"
        <*> v .:? "signature"
        <*> v .:? "signer"
