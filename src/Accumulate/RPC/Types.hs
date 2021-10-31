{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Accumulate.RPC.Types where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Maybe
import qualified Data.Text        as T
import           Data.Time
import           GHC.Generics


--------------------------------------------------------------------------------

data ApiRequest =
  ApiRequest
    { jsonRpc :: T.Text
    , id      :: Int
    , method  :: T.Text
    , params  :: Maybe Object
    }
  deriving (Eq, Show, Generic, ToJSON)

data ApiResponse a =
  ApiResponse
    { jsonRpc :: T.Text
    , id      :: Int
    , result  :: a
    }
  deriving (Eq, Show, Generic, FromJSON)
