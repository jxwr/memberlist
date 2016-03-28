{-# LANGUAGE DeriveGeneric #-}

module PingMsg where

import GHC.Generics
import Data.Aeson
import Data.Text

data PingMsg = PingMsg {
  seqNo :: Int,
  node :: !Text
  } deriving (Show, Generic)

instance FromJSON PingMsg
instance ToJSON PingMsg

