module Struct where

import qualified Data.Map as Map  
import Control.Concurrent.MVar

type SeqNo = Int

data AckMessage
  = AckMessage
    { complete :: Bool
    }
  deriving (Show)

data Message
  = AckResp SeqNo
  | Ping SeqNo String
  | IndirectPingReq SeqNo
  | Suspect Int String String
  | Alive Int String String Int
  | Dead Int String String
  deriving (Show)

data Timeout
  = Timeout

data NodeStateType
  = StateAlive
  | StateSuspect
  | StateDead
  deriving (Show)

data Node
  = Node
    { name :: String
    , addr :: String
    , port :: Int
    }
  deriving (Show)

data NodeState
  = NodeState
    { node :: Node
    , incarnation :: Int
    , state :: NodeStateType
    , stateChange :: Int
    }
  deriving (Show)

data Config
  = Config
    { indirectChecks :: Int
    , probeInterval :: Int
    , probeTimeout :: Int
    , nodeName :: String
    , nodeAddr :: String
    , nodePort :: Int
    }
  deriving (Show)

type AckHandler
  = AckMessage -> IO ()

data Memberlist
  = Memberlist
    { config :: Config
    , sequenceNum :: Int
    , ackHandlers :: MVar (Map.Map Int AckHandler)
    , nodes :: MVar [NodeState]
    , nodeMap :: MVar (Map.Map String NodeState)
    }
    
instance Show Memberlist where
  show m = "Memberlist { " ++
    (show (config m)) ++
    ", " ++ (show (sequenceNum m)) ++
    " }"

