{-# LANGUAGE DeriveGeneric #-}

module IndirectPingMsg where

import GHC.Generics
import Data.Aeson
import Data.Text

data IndirectPingMsg = IndirectPingMsg {
  seqNo :: Int,
  target :: !Text,
  port :: Int,
  node :: !Text
  } deriving (Show, Generic)


instance FromJSON IndirectPingMsg
instance ToJSON IndirectPingMsg
