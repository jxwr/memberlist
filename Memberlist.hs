{-# LANGUAGE LambdaCase #-}

import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import qualified Data.Map as Map  

import Util
import Struct
import State
import BuggySelect

schedule :: Memberlist -> IO ()
schedule memberlist = do
  -- probe
  go $ forever $ do
    threadDelay $ 1 * second
    probe memberlist
  return ()

  gossip memberlist


setAlive :: Memberlist -> IO Bool
setAlive m = do
  let conf = (config m)
  let alive = Alive 0 (nodeName conf) (nodeAddr conf) (nodePort conf)
  
  aliveNode m alive
  

create :: Config -> IO Memberlist
create config = do
  handlerMap <- newMVar Map.empty
  nodes <- newMVar []
  nodeMap <- newMVar Map.empty
  
  let m = Memberlist
        { config = config
        , sequenceNum = 0
        , ackHandlers = handlerMap
        , nodes = nodes
        , nodeMap = nodeMap
        }
        
  setAlive m
  
  schedule m

  -- testinig msg
  go $ forever $ do
    threadDelay $ 2 * second
    handlers <- takeMVar handlerMap
    case Map.lookup 1 handlers of
      Just f -> do
        putStrLn "===> send msg"
        f $ AckMessage False
      Nothing -> putStrLn "nothing"
  
  return m


run = do
  let config = Config { indirectChecks = 3
                      , probeInterval = 10
                      , probeTimeout = 5
                      , nodeName = "n0"
                      , nodeAddr = "127.0.0.1"
                      , nodePort = 8801
                      }
        
  create config
