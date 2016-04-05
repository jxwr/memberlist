{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module AckRespMsg where

import GHC.Generics
import Data.Aeson
import Data.Text

data AckRespMsg = AckRespMsg {
  seqNo :: Int,
  payload :: !Text
  } deriving (Show, Generic)

instance FromJSON AckRespMsg
instance ToJSON AckRespMsg
