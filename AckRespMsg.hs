{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import GHC.Generics
import Data.Aeson
import Data.Text

data AckRespMsg = AckRespMsg {
  seqNo :: Int,
  payload :: !Text
  } deriving (Show, Generic)

instance FromJSON AckRespMsg
instance ToJSON AckRespMsg
