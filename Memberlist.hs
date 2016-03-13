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

run = do
  let config = Config { indirectChecks = 3
                      , probeInterval = 10
                      , probeTimeout = 5
                      , nodeName = "n0"
                      }

  handlerMap <- newMVar Map.empty
  let memberlist = Memberlist { config = config
                              , sequenceNum = 0
                              , ackHandlers = handlerMap
                              }

  -- testinig msg
  go $ forever $ do
    threadDelay $ 2 * second
    handlers <- takeMVar handlerMap
    case Map.lookup 1 handlers of
      Just f -> do
        putStrLn "===> send msg"
        f $ AckMessage False
      Nothing -> putStrLn "nothing"
        
  schedule memberlist
  gossip memberlist


