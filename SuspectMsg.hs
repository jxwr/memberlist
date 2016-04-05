{-# LANGUAGE DeriveGeneric #-}

module SuspectMsg where

import GHC.Generics
import Data.Aeson
import Data.Text

data SuspectMsg = SuspectMsg {
  incarnation :: Int,
  node :: !Text,
  from :: !Text
  } deriving (Show, Generic)


instance FromJSON SuspectMsg
instance ToJSON SuspectMsg
