{-# LANGUAGE DeriveGeneric #-}

module DeadMsg where

import GHC.Generics
import Data.Aeson
import Data.Text

data DeadMsg = DeadMsg {
  incarnation :: Int,
  node :: !Text,
  from :: !Text
  } deriving (Show, Generic)


instance FromJSON DeadMsg
instance ToJSON DeadMsg
