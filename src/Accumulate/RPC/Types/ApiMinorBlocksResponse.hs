{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Accumulate.RPC.Types.ApiMinorBlocksResponse
    ( APIMinorBlocksResponse (..)
    , Item (..)
    , TransactionElement (..)
    , SignatureBook (..)
    , Page (..)
    , Signature (..)
    , Signer (..)
    , Status (..)
    , ResultClass (..)
    , TransactionTransaction (..)
    , Data (..)
    , DataEntry (..)
    , Receipt (..)
    , Anchor (..)
    , RootChainReceipt (..)
    , EntryElement (..)
    , Header (..)
    , decodeTopLevel
    ) where

import           Data.Aeson
import           Data.Aeson.Types     (emptyObject)
import           Data.ByteString.Lazy (ByteString)
import           Data.HashMap.Strict  (HashMap)
import           Data.Text            (Text)

data APIMinorBlocksResponse = APIMinorBlocksResponse
    { itemsAPIMinorBlocksResponse                      :: Maybe ([Item])
    , apiMinorBlocksResponseTypeAPIMinorBlocksResponse :: Maybe Text
    , totalAPIMinorBlocksResponse                      :: Maybe Int
    , startAPIMinorBlocksResponse                      :: Maybe Int
    , countAPIMinorBlocksResponse                      :: Maybe Int
    } deriving (Show)

data Item = Item
    { txIDSItem        :: Maybe ([Text])
    , transactionsItem :: Maybe ([TransactionElement])
    , blockIndexItem   :: Maybe Int
    , blockTimeItem    :: Maybe Text
    , txCountItem      :: Maybe Int
    } deriving (Show)

data TransactionElement = TransactionElement
    { txidTransactionElement            :: Maybe Text
    , transactionTypeTransactionElement :: Maybe Text
    , sponsorTransactionElement         :: Maybe Text
    , statusTransactionElement          :: Maybe Status
    , transactionTransactionElement     :: Maybe TransactionTransaction
    , transactionHashTransactionElement :: Maybe Text
    , transactionDataTransactionElement :: Maybe Data
    , originTransactionElement          :: Maybe Text
    , signaturesTransactionElement      :: Maybe ([Signature])
    , signatureBooksTransactionElement  :: Maybe ([SignatureBook])
    } deriving (Show)

data SignatureBook = SignatureBook
    { authoritySignatureBook :: Maybe Text
    , pagesSignatureBook     :: Maybe ([Page])
    } deriving (Show)

data Page = Page
    { signaturesPage :: Maybe ([Signature])
    , signerPage     :: Maybe Signer
    } deriving (Show)

data Signature = Signature
    { signatureTypeSignature      :: Maybe Text
    , destinationNetworkSignature :: Maybe Text
    , sourceNetworkSignature      :: Maybe Text
    , sequenceNumberSignature     :: Maybe Int
    , transactionHashSignature    :: Maybe Text
    , publicKeySignature          :: Maybe Text
    , timestampSignature          :: Maybe Int
    , signatureSignature          :: Maybe Text
    , signerSignature             :: Maybe Text
    } deriving (Show)

data Signer = Signer
    { signerTypeSigner :: Maybe Text
    , urlSigner        :: Maybe Text
    } deriving (Show)

data Status = Status
    { txIDStatus               :: Maybe Text
    , codeNumStatus            :: Maybe Int
    , deliveredStatus          :: Maybe Bool
    , codeStatus               :: Maybe Text
    , resultStatus             :: Maybe ResultClass
    , signersStatus            :: Maybe ([Signer])
    , destinationNetworkStatus :: Maybe Text
    , receivedStatus           :: Maybe Int
    , sourceNetworkStatus      :: Maybe Text
    , anchorSignersStatus      :: Maybe ([Text])
    , sequenceNumberStatus     :: Maybe Int
    , initiatorStatus          :: Maybe Text
    } deriving (Show)

data ResultClass = ResultClass
    { resultTypeResultClass :: Maybe Text
    } deriving (Show)

data TransactionTransaction = TransactionTransaction
    { headerTransactionTransaction :: Maybe Header
    , bodyTransactionTransaction   :: Maybe Data
    } deriving (Show)

data Data = Data
    { dataTypeData           :: Maybe Text
    , entryData              :: Maybe DataEntry
    , sourceData             :: Maybe Text
    , makeMajorBlockTimeData :: Maybe Text
    , stateTreeAnchorData    :: Maybe Text
    , minorBlockIndexData    :: Maybe Int
    , rootChainIndexData     :: Maybe Int
    , rootChainAnchorData    :: Maybe Text
    , receiptsData           :: Maybe ([Receipt])
    } deriving (Show)

data DataEntry = DataEntry
    { entryTypeDataEntry :: Maybe Text
    , entryDataDataEntry :: Maybe ([Text])
    } deriving (Show)

data Receipt = Receipt
    { anchorReceipt           :: Maybe Anchor
    , rootChainReceiptReceipt :: Maybe RootChainReceipt
    } deriving (Show)

data Anchor = Anchor
    { sourceAnchor          :: Maybe Text
    , stateTreeAnchorAnchor :: Maybe Text
    , minorBlockIndexAnchor :: Maybe Int
    , rootChainIndexAnchor  :: Maybe Int
    , rootChainAnchorAnchor :: Maybe Text
    } deriving (Show)

data RootChainReceipt = RootChainReceipt
    { startRootChainReceipt   :: Maybe Text
    , endRootChainReceipt     :: Maybe Text
    , anchorRootChainReceipt  :: Maybe Text
    , entriesRootChainReceipt :: Maybe ([EntryElement])
    } deriving (Show)

data EntryElement = EntryElement
    { hashEntryElement  :: Maybe Text
    , rightEntryElement :: Maybe Bool
    } deriving (Show)

data Header = Header
    { principalHeader :: Maybe Text
    , initiatorHeader :: Maybe Text
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe APIMinorBlocksResponse
decodeTopLevel = decode

instance ToJSON APIMinorBlocksResponse where
    toJSON (APIMinorBlocksResponse itemsAPIMinorBlocksResponse apiMinorBlocksResponseTypeAPIMinorBlocksResponse totalAPIMinorBlocksResponse startAPIMinorBlocksResponse countAPIMinorBlocksResponse) =
        object
        [ "items" .= itemsAPIMinorBlocksResponse
        , "type" .= apiMinorBlocksResponseTypeAPIMinorBlocksResponse
        , "total" .= totalAPIMinorBlocksResponse
        , "start" .= startAPIMinorBlocksResponse
        , "count" .= countAPIMinorBlocksResponse
        ]

instance FromJSON APIMinorBlocksResponse where
    parseJSON (Object v) = APIMinorBlocksResponse
        <$> v .:? "items"
        <*> v .:? "type"
        <*> v .:? "total"
        <*> v .:? "start"
        <*> v .:? "count"

instance ToJSON Item where
    toJSON (Item txIDSItem transactionsItem blockIndexItem blockTimeItem txCountItem) =
        object
        [ "txIds" .= txIDSItem
        , "transactions" .= transactionsItem
        , "blockIndex" .= blockIndexItem
        , "blockTime" .= blockTimeItem
        , "txCount" .= txCountItem
        ]

instance FromJSON Item where
    parseJSON (Object v) = Item
        <$> v .:? "txIds"
        <*> v .:? "transactions"
        <*> v .:? "blockIndex"
        <*> v .:? "blockTime"
        <*> v .:? "txCount"

instance ToJSON TransactionElement where
    toJSON (TransactionElement txidTransactionElement transactionTypeTransactionElement sponsorTransactionElement statusTransactionElement transactionTransactionElement transactionHashTransactionElement transactionDataTransactionElement originTransactionElement signaturesTransactionElement signatureBooksTransactionElement) =
        object
        [ "txid" .= txidTransactionElement
        , "type" .= transactionTypeTransactionElement
        , "sponsor" .= sponsorTransactionElement
        , "status" .= statusTransactionElement
        , "transaction" .= transactionTransactionElement
        , "transactionHash" .= transactionHashTransactionElement
        , "data" .= transactionDataTransactionElement
        , "origin" .= originTransactionElement
        , "signatures" .= signaturesTransactionElement
        , "signatureBooks" .= signatureBooksTransactionElement
        ]

instance FromJSON TransactionElement where
    parseJSON (Object v) = TransactionElement
        <$> v .:? "txid"
        <*> v .:? "type"
        <*> v .:? "sponsor"
        <*> v .:? "status"
        <*> v .:? "transaction"
        <*> v .:? "transactionHash"
        <*> v .:? "data"
        <*> v .:? "origin"
        <*> v .:? "signatures"
        <*> v .:? "signatureBooks"

instance ToJSON SignatureBook where
    toJSON (SignatureBook authoritySignatureBook pagesSignatureBook) =
        object
        [ "authority" .= authoritySignatureBook
        , "pages" .= pagesSignatureBook
        ]

instance FromJSON SignatureBook where
    parseJSON (Object v) = SignatureBook
        <$> v .:? "authority"
        <*> v .:? "pages"

instance ToJSON Page where
    toJSON (Page signaturesPage signerPage) =
        object
        [ "signatures" .= signaturesPage
        , "signer" .= signerPage
        ]

instance FromJSON Page where
    parseJSON (Object v) = Page
        <$> v .:? "signatures"
        <*> v .:? "signer"

instance ToJSON Signature where
    toJSON (Signature signatureTypeSignature destinationNetworkSignature sourceNetworkSignature sequenceNumberSignature transactionHashSignature publicKeySignature timestampSignature signatureSignature signerSignature) =
        object
        [ "type" .= signatureTypeSignature
        , "destinationNetwork" .= destinationNetworkSignature
        , "sourceNetwork" .= sourceNetworkSignature
        , "sequenceNumber" .= sequenceNumberSignature
        , "transactionHash" .= transactionHashSignature
        , "publicKey" .= publicKeySignature
        , "timestamp" .= timestampSignature
        , "signature" .= signatureSignature
        , "signer" .= signerSignature
        ]

instance FromJSON Signature where
    parseJSON (Object v) = Signature
        <$> v .:? "type"
        <*> v .:? "destinationNetwork"
        <*> v .:? "sourceNetwork"
        <*> v .:? "sequenceNumber"
        <*> v .:? "transactionHash"
        <*> v .:? "publicKey"
        <*> v .:? "timestamp"
        <*> v .:? "signature"
        <*> v .:? "signer"

instance ToJSON Signer where
    toJSON (Signer signerTypeSigner urlSigner) =
        object
        [ "type" .= signerTypeSigner
        , "url" .= urlSigner
        ]

instance FromJSON Signer where
    parseJSON (Object v) = Signer
        <$> v .:? "type"
        <*> v .:? "url"

instance ToJSON Status where
    toJSON (Status txIDStatus codeNumStatus deliveredStatus codeStatus resultStatus signersStatus destinationNetworkStatus receivedStatus sourceNetworkStatus anchorSignersStatus sequenceNumberStatus initiatorStatus) =
        object
        [ "txID" .= txIDStatus
        , "codeNum" .= codeNumStatus
        , "delivered" .= deliveredStatus
        , "code" .= codeStatus
        , "result" .= resultStatus
        , "signers" .= signersStatus
        , "destinationNetwork" .= destinationNetworkStatus
        , "received" .= receivedStatus
        , "sourceNetwork" .= sourceNetworkStatus
        , "anchorSigners" .= anchorSignersStatus
        , "sequenceNumber" .= sequenceNumberStatus
        , "initiator" .= initiatorStatus
        ]

instance FromJSON Status where
    parseJSON (Object v) = Status
        <$> v .:? "txID"
        <*> v .:? "codeNum"
        <*> v .:? "delivered"
        <*> v .:? "code"
        <*> v .:? "result"
        <*> v .:? "signers"
        <*> v .:? "destinationNetwork"
        <*> v .:? "received"
        <*> v .:? "sourceNetwork"
        <*> v .:? "anchorSigners"
        <*> v .:? "sequenceNumber"
        <*> v .:? "initiator"

instance ToJSON ResultClass where
    toJSON (ResultClass resultTypeResultClass) =
        object
        [ "type" .= resultTypeResultClass
        ]

instance FromJSON ResultClass where
    parseJSON (Object v) = ResultClass
        <$> v .:? "type"

instance ToJSON TransactionTransaction where
    toJSON (TransactionTransaction headerTransactionTransaction bodyTransactionTransaction) =
        object
        [ "header" .= headerTransactionTransaction
        , "body" .= bodyTransactionTransaction
        ]

instance FromJSON TransactionTransaction where
    parseJSON (Object v) = TransactionTransaction
        <$> v .:? "header"
        <*> v .:? "body"

instance ToJSON Data where
    toJSON (Data dataTypeData entryData sourceData makeMajorBlockTimeData stateTreeAnchorData minorBlockIndexData rootChainIndexData rootChainAnchorData receiptsData) =
        object
        [ "type" .= dataTypeData
        , "entry" .= entryData
        , "source" .= sourceData
        , "makeMajorBlockTime" .= makeMajorBlockTimeData
        , "stateTreeAnchor" .= stateTreeAnchorData
        , "minorBlockIndex" .= minorBlockIndexData
        , "rootChainIndex" .= rootChainIndexData
        , "rootChainAnchor" .= rootChainAnchorData
        , "receipts" .= receiptsData
        ]

instance FromJSON Data where
    parseJSON (Object v) = Data
        <$> v .:? "type"
        <*> v .:? "entry"
        <*> v .:? "source"
        <*> v .:? "makeMajorBlockTime"
        <*> v .:? "stateTreeAnchor"
        <*> v .:? "minorBlockIndex"
        <*> v .:? "rootChainIndex"
        <*> v .:? "rootChainAnchor"
        <*> v .:? "receipts"

instance ToJSON DataEntry where
    toJSON (DataEntry entryTypeDataEntry entryDataDataEntry) =
        object
        [ "type" .= entryTypeDataEntry
        , "data" .= entryDataDataEntry
        ]

instance FromJSON DataEntry where
    parseJSON (Object v) = DataEntry
        <$> v .:? "type"
        <*> v .:? "data"

instance ToJSON Receipt where
    toJSON (Receipt anchorReceipt rootChainReceiptReceipt) =
        object
        [ "anchor" .= anchorReceipt
        , "rootChainReceipt" .= rootChainReceiptReceipt
        ]

instance FromJSON Receipt where
    parseJSON (Object v) = Receipt
        <$> v .:? "anchor"
        <*> v .:? "rootChainReceipt"

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
        <$> v .:? "source"
        <*> v .:? "stateTreeAnchor"
        <*> v .:? "minorBlockIndex"
        <*> v .:? "rootChainIndex"
        <*> v .:? "rootChainAnchor"

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
        <$> v .:? "start"
        <*> v .:? "end"
        <*> v .:? "anchor"
        <*> v .:? "entries"

instance ToJSON EntryElement where
    toJSON (EntryElement hashEntryElement rightEntryElement) =
        object
        [ "hash" .= hashEntryElement
        , "right" .= rightEntryElement
        ]

instance FromJSON EntryElement where
    parseJSON (Object v) = EntryElement
        <$> v .:? "hash"
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







--------------------------------
--- MAINNET


-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE StrictData        #-}

-- module Accumulate.RPC.Types.ApiMinorBlocksResponse
--     ( APIMinorBlocksResponse (..)
--     , Item (..)
--     , TransactionElement (..)
--     , SignatureBook (..)
--     , Page (..)
--     , Signature (..)
--     , Signer (..)
--     , Status (..)
--     , ResultClass (..)
--     , TransactionTransaction (..)
--     , Data (..)
--     , Receipt (..)
--     , Anchor (..)
--     , RootChainReceipt (..)
--     , Entry (..)
--     , Header (..)
--     , decodeTopLevel
--     ) where

-- import           Data.Aeson
-- import           Data.Aeson.Types     (emptyObject)
-- import           Data.ByteString.Lazy (ByteString)
-- import           Data.HashMap.Strict  (HashMap)
-- import           Data.Text            (Text)

-- data APIMinorBlocksResponse = APIMinorBlocksResponse
--     { itemsAPIMinorBlocksResponse                      :: [Item]
--     , apiMinorBlocksResponseTypeAPIMinorBlocksResponse :: Text
--     , totalAPIMinorBlocksResponse                      :: Int
--     , startAPIMinorBlocksResponse                      :: Int
--     , countAPIMinorBlocksResponse                      :: Int
--     } deriving (Show)

-- data Item = Item
--     { blockIndexItem   :: Int
--     , blockTimeItem    :: Maybe Text
--     , txIDSItem        :: Maybe ([Text])
--     , transactionsItem :: Maybe ([TransactionElement])
--     , txCountItem      :: Maybe Int
--     } deriving (Show)

-- data TransactionElement = TransactionElement
--     { txidTransactionElement            :: Text
--     , transactionTypeTransactionElement :: Text
--     , sponsorTransactionElement         :: Text
--     , signaturesTransactionElement      :: [Signature]
--     , statusTransactionElement          :: Status
--     , signatureBooksTransactionElement  :: [SignatureBook]
--     , transactionTransactionElement     :: TransactionTransaction
--     , transactionHashTransactionElement :: Text
--     , transactionDataTransactionElement :: Data
--     , originTransactionElement          :: Text
--     } deriving (Show)

-- data SignatureBook = SignatureBook
--     { authoritySignatureBook :: Text
--     , pagesSignatureBook     :: [Page]
--     } deriving (Show)

-- data Page = Page
--     { signaturesPage :: [Signature]
--     , signerPage     :: Signer
--     } deriving (Show)

-- data Signature = Signature
--     { signatureTypeSignature      :: Text
--     , destinationNetworkSignature :: Maybe Text
--     , sourceNetworkSignature      :: Maybe Text
--     , sequenceNumberSignature     :: Maybe Int
--     , transactionHashSignature    :: Text
--     , publicKeySignature          :: Maybe Text
--     , timestampSignature          :: Maybe Int
--     , signatureSignature          :: Maybe Text
--     , signerSignature             :: Maybe Text
--     } deriving (Show)

-- data Signer = Signer
--     { signerTypeSigner :: Text
--     , urlSigner        :: Text
--     } deriving (Show)

-- data Status = Status
--     { txIDStatus               :: Text
--     , codeNumStatus            :: Int
--     , deliveredStatus          :: Bool
--     , codeStatus               :: Text
--     , signersStatus            :: [Signer]
--     , destinationNetworkStatus :: Text
--     , receivedStatus           :: Int
--     , sourceNetworkStatus      :: Text
--     , anchorSignersStatus      :: [Text]
--     , sequenceNumberStatus     :: Int
--     , resultStatus             :: ResultClass
--     , initiatorStatus          :: Text
--     } deriving (Show)

-- data ResultClass = ResultClass
--     { resultTypeResultClass :: Text
--     } deriving (Show)

-- data TransactionTransaction = TransactionTransaction
--     { headerTransactionTransaction :: Header
--     , bodyTransactionTransaction   :: Data
--     } deriving (Show)

-- data Data = Data
--     { dataTypeData           :: Text
--     , sourceData             :: Text
--     , makeMajorBlockTimeData :: Text
--     , stateTreeAnchorData    :: Text
--     , minorBlockIndexData    :: Int
--     , rootChainIndexData     :: Int
--     , rootChainAnchorData    :: Text
--     , receiptsData           :: Maybe ([Receipt])
--     } deriving (Show)

-- data Receipt = Receipt
--     { anchorReceipt           :: Anchor
--     , rootChainReceiptReceipt :: RootChainReceipt
--     } deriving (Show)

-- data Anchor = Anchor
--     { sourceAnchor          :: Text
--     , stateTreeAnchorAnchor :: Text
--     , minorBlockIndexAnchor :: Int
--     , rootChainIndexAnchor  :: Int
--     , rootChainAnchorAnchor :: Text
--     } deriving (Show)

-- data RootChainReceipt = RootChainReceipt
--     { startRootChainReceipt   :: Text
--     , endRootChainReceipt     :: Text
--     , anchorRootChainReceipt  :: Text
--     , entriesRootChainReceipt :: [Entry]
--     } deriving (Show)

-- data Entry = Entry
--     { hashEntry  :: Text
--     , rightEntry :: Maybe Bool
--     } deriving (Show)

-- data Header = Header
--     { principalHeader :: Text
--     , initiatorHeader :: Text
--     } deriving (Show)

-- decodeTopLevel :: ByteString -> Maybe APIMinorBlocksResponse
-- decodeTopLevel = decode

-- instance ToJSON APIMinorBlocksResponse where
--     toJSON (APIMinorBlocksResponse itemsAPIMinorBlocksResponse apiMinorBlocksResponseTypeAPIMinorBlocksResponse totalAPIMinorBlocksResponse startAPIMinorBlocksResponse countAPIMinorBlocksResponse) =
--         object
--         [ "items" .= itemsAPIMinorBlocksResponse
--         , "type" .= apiMinorBlocksResponseTypeAPIMinorBlocksResponse
--         , "total" .= totalAPIMinorBlocksResponse
--         , "start" .= startAPIMinorBlocksResponse
--         , "count" .= countAPIMinorBlocksResponse
--         ]

-- instance FromJSON APIMinorBlocksResponse where
--     parseJSON (Object v) = APIMinorBlocksResponse
--         <$> v .: "items"
--         <*> v .: "type"
--         <*> v .: "total"
--         <*> v .: "start"
--         <*> v .: "count"

-- instance ToJSON Item where
--     toJSON (Item blockIndexItem blockTimeItem txIDSItem transactionsItem txCountItem) =
--         object
--         [ "blockIndex" .= blockIndexItem
--         , "blockTime" .= blockTimeItem
--         , "txIds" .= txIDSItem
--         , "transactions" .= transactionsItem
--         , "txCount" .= txCountItem
--         ]

-- instance FromJSON Item where
--     parseJSON (Object v) = Item
--         <$> v .: "blockIndex"
--         <*> v .:? "blockTime"
--         <*> v .:? "txIds"
--         <*> v .:? "transactions"
--         <*> v .:? "txCount"

-- instance ToJSON TransactionElement where
--     toJSON (TransactionElement txidTransactionElement transactionTypeTransactionElement sponsorTransactionElement signaturesTransactionElement statusTransactionElement signatureBooksTransactionElement transactionTransactionElement transactionHashTransactionElement transactionDataTransactionElement originTransactionElement) =
--         object
--         [ "txid" .= txidTransactionElement
--         , "type" .= transactionTypeTransactionElement
--         , "sponsor" .= sponsorTransactionElement
--         , "signatures" .= signaturesTransactionElement
--         , "status" .= statusTransactionElement
--         , "signatureBooks" .= signatureBooksTransactionElement
--         , "transaction" .= transactionTransactionElement
--         , "transactionHash" .= transactionHashTransactionElement
--         , "data" .= transactionDataTransactionElement
--         , "origin" .= originTransactionElement
--         ]

-- instance FromJSON TransactionElement where
--     parseJSON (Object v) = TransactionElement
--         <$> v .: "txid"
--         <*> v .: "type"
--         <*> v .: "sponsor"
--         <*> v .: "signatures"
--         <*> v .: "status"
--         <*> v .: "signatureBooks"
--         <*> v .: "transaction"
--         <*> v .: "transactionHash"
--         <*> v .: "data"
--         <*> v .: "origin"

-- instance ToJSON SignatureBook where
--     toJSON (SignatureBook authoritySignatureBook pagesSignatureBook) =
--         object
--         [ "authority" .= authoritySignatureBook
--         , "pages" .= pagesSignatureBook
--         ]

-- instance FromJSON SignatureBook where
--     parseJSON (Object v) = SignatureBook
--         <$> v .: "authority"
--         <*> v .: "pages"

-- instance ToJSON Page where
--     toJSON (Page signaturesPage signerPage) =
--         object
--         [ "signatures" .= signaturesPage
--         , "signer" .= signerPage
--         ]

-- instance FromJSON Page where
--     parseJSON (Object v) = Page
--         <$> v .: "signatures"
--         <*> v .: "signer"

-- instance ToJSON Signature where
--     toJSON (Signature signatureTypeSignature destinationNetworkSignature sourceNetworkSignature sequenceNumberSignature transactionHashSignature publicKeySignature timestampSignature signatureSignature signerSignature) =
--         object
--         [ "type" .= signatureTypeSignature
--         , "destinationNetwork" .= destinationNetworkSignature
--         , "sourceNetwork" .= sourceNetworkSignature
--         , "sequenceNumber" .= sequenceNumberSignature
--         , "transactionHash" .= transactionHashSignature
--         , "publicKey" .= publicKeySignature
--         , "timestamp" .= timestampSignature
--         , "signature" .= signatureSignature
--         , "signer" .= signerSignature
--         ]

-- instance FromJSON Signature where
--     parseJSON (Object v) = Signature
--         <$> v .: "type"
--         <*> v .:? "destinationNetwork"
--         <*> v .:? "sourceNetwork"
--         <*> v .:? "sequenceNumber"
--         <*> v .: "transactionHash"
--         <*> v .:? "publicKey"
--         <*> v .:? "timestamp"
--         <*> v .:? "signature"
--         <*> v .:? "signer"

-- instance ToJSON Signer where
--     toJSON (Signer signerTypeSigner urlSigner) =
--         object
--         [ "type" .= signerTypeSigner
--         , "url" .= urlSigner
--         ]

-- instance FromJSON Signer where
--     parseJSON (Object v) = Signer
--         <$> v .: "type"
--         <*> v .: "url"

-- instance ToJSON Status where
--     toJSON (Status txIDStatus codeNumStatus deliveredStatus codeStatus signersStatus destinationNetworkStatus receivedStatus sourceNetworkStatus anchorSignersStatus sequenceNumberStatus resultStatus initiatorStatus) =
--         object
--         [ "txID" .= txIDStatus
--         , "codeNum" .= codeNumStatus
--         , "delivered" .= deliveredStatus
--         , "code" .= codeStatus
--         , "signers" .= signersStatus
--         , "destinationNetwork" .= destinationNetworkStatus
--         , "received" .= receivedStatus
--         , "sourceNetwork" .= sourceNetworkStatus
--         , "anchorSigners" .= anchorSignersStatus
--         , "sequenceNumber" .= sequenceNumberStatus
--         , "result" .= resultStatus
--         , "initiator" .= initiatorStatus
--         ]

-- instance FromJSON Status where
--     parseJSON (Object v) = Status
--         <$> v .: "txID"
--         <*> v .: "codeNum"
--         <*> v .: "delivered"
--         <*> v .: "code"
--         <*> v .: "signers"
--         <*> v .: "destinationNetwork"
--         <*> v .: "received"
--         <*> v .: "sourceNetwork"
--         <*> v .: "anchorSigners"
--         <*> v .: "sequenceNumber"
--         <*> v .: "result"
--         <*> v .: "initiator"

-- instance ToJSON ResultClass where
--     toJSON (ResultClass resultTypeResultClass) =
--         object
--         [ "type" .= resultTypeResultClass
--         ]

-- instance FromJSON ResultClass where
--     parseJSON (Object v) = ResultClass
--         <$> v .: "type"

-- instance ToJSON TransactionTransaction where
--     toJSON (TransactionTransaction headerTransactionTransaction bodyTransactionTransaction) =
--         object
--         [ "header" .= headerTransactionTransaction
--         , "body" .= bodyTransactionTransaction
--         ]

-- instance FromJSON TransactionTransaction where
--     parseJSON (Object v) = TransactionTransaction
--         <$> v .: "header"
--         <*> v .: "body"

-- instance ToJSON Data where
--     toJSON (Data dataTypeData sourceData makeMajorBlockTimeData stateTreeAnchorData minorBlockIndexData rootChainIndexData rootChainAnchorData receiptsData) =
--         object
--         [ "type" .= dataTypeData
--         , "source" .= sourceData
--         , "makeMajorBlockTime" .= makeMajorBlockTimeData
--         , "stateTreeAnchor" .= stateTreeAnchorData
--         , "minorBlockIndex" .= minorBlockIndexData
--         , "rootChainIndex" .= rootChainIndexData
--         , "rootChainAnchor" .= rootChainAnchorData
--         , "receipts" .= receiptsData
--         ]

-- instance FromJSON Data where
--     parseJSON (Object v) = Data
--         <$> v .: "type"
--         <*> v .: "source"
--         <*> v .: "makeMajorBlockTime"
--         <*> v .: "stateTreeAnchor"
--         <*> v .: "minorBlockIndex"
--         <*> v .: "rootChainIndex"
--         <*> v .: "rootChainAnchor"
--         <*> v .:? "receipts"

-- instance ToJSON Receipt where
--     toJSON (Receipt anchorReceipt rootChainReceiptReceipt) =
--         object
--         [ "anchor" .= anchorReceipt
--         , "rootChainReceipt" .= rootChainReceiptReceipt
--         ]

-- instance FromJSON Receipt where
--     parseJSON (Object v) = Receipt
--         <$> v .: "anchor"
--         <*> v .: "rootChainReceipt"

-- instance ToJSON Anchor where
--     toJSON (Anchor sourceAnchor stateTreeAnchorAnchor minorBlockIndexAnchor rootChainIndexAnchor rootChainAnchorAnchor) =
--         object
--         [ "source" .= sourceAnchor
--         , "stateTreeAnchor" .= stateTreeAnchorAnchor
--         , "minorBlockIndex" .= minorBlockIndexAnchor
--         , "rootChainIndex" .= rootChainIndexAnchor
--         , "rootChainAnchor" .= rootChainAnchorAnchor
--         ]

-- instance FromJSON Anchor where
--     parseJSON (Object v) = Anchor
--         <$> v .: "source"
--         <*> v .: "stateTreeAnchor"
--         <*> v .: "minorBlockIndex"
--         <*> v .: "rootChainIndex"
--         <*> v .: "rootChainAnchor"

-- instance ToJSON RootChainReceipt where
--     toJSON (RootChainReceipt startRootChainReceipt endRootChainReceipt anchorRootChainReceipt entriesRootChainReceipt) =
--         object
--         [ "start" .= startRootChainReceipt
--         , "end" .= endRootChainReceipt
--         , "anchor" .= anchorRootChainReceipt
--         , "entries" .= entriesRootChainReceipt
--         ]

-- instance FromJSON RootChainReceipt where
--     parseJSON (Object v) = RootChainReceipt
--         <$> v .: "start"
--         <*> v .: "end"
--         <*> v .: "anchor"
--         <*> v .: "entries"

-- instance ToJSON Entry where
--     toJSON (Entry hashEntry rightEntry) =
--         object
--         [ "hash" .= hashEntry
--         , "right" .= rightEntry
--         ]

-- instance FromJSON Entry where
--     parseJSON (Object v) = Entry
--         <$> v .: "hash"
--         <*> v .:? "right"

-- instance ToJSON Header where
--     toJSON (Header principalHeader initiatorHeader) =
--         object
--         [ "principal" .= principalHeader
--         , "initiator" .= initiatorHeader
--         ]

-- instance FromJSON Header where
--     parseJSON (Object v) = Header
--         <$> v .: "principal"
--         <*> v .: "initiator"
