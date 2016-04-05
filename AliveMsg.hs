{-# LANGUAGE DeriveGeneric #-}

module AliveMsg where

import GHC.Generics
import Data.Aeson
import Data.Text

data AliveMsg = AliveMsg {
  incarnation :: Int,
  addr :: !Text,
  node :: !Text,
  port :: Int,
  meta :: !Text
  } deriving (Show, Generic)

instance FromJSON AliveMsg
instance ToJSON AliveMsg

