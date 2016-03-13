{-# LANGUAGE LambdaCase #-}

module State where

import Struct
import Net
import Util
import BuggySelect

import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.State.Lazy (StateT, put, get, lift, evalStateT);
import qualified Data.Map as Map  

setAckChannel :: Int -> Chan AckMessage -> Int -> StateT Memberlist IO ()
setAckChannel seq ch interval = do
  m <- get
  lift $ do
    handlerMap <- takeMVar $ ackHandlers m
    let ackHandler msg = writeChan ch msg
    putMVar (ackHandlers m) (Map.insert seq ackHandler handlerMap)

sendMsg node msg = do
  putStrLn $ "send " ++ (show msg) ++ " to " ++ node

suspectNode node = do
  putStrLn $ "suspect " ++ (show node)

aliveNode :: Memberlist -> Message -> IO Bool
aliveNode m (Alive _ nodeName nodeAddr nodePort) = do
  let nodeMapMVar = (nodeMap m)
  nodeMap <- takeMVar nodeMapMVar
  -- Check if we've never seen this node before, and if not, then
  -- store this node in our node map.
  case (Map.lookup nodeName nodeMap) of
    Just nodeState -> return False
    Nothing -> do
      let node = Node
            { name = nodeName
            , addr = nodeAddr
            , port = nodePort
            }
      let ns = NodeState
            { node = node
            , incarnation = 0
            , state = StateAlive
            , stateChange = 1
            }

      -- Add to map
      putStrLn $ "put node:" ++ nodeName
      putMVar nodeMapMVar (Map.insert nodeName ns nodeMap)

      -- Add at the end and swap with the node at the offset
      let nodesMVar = nodes m
      nodeList <- takeMVar nodesMVar
      putMVar nodesMVar (ns:nodeList)
      return True

probeNode :: Node -> StateT Memberlist IO ()
probeNode node = do
  m <- get
  let conf = config m

  seqNo <- nextSeqNo
  -- Prepare a ping message and setup an ack handler
  let ping = Ping seqNo (name node)
  ackCh <- lift newChan
  setAckChannel seqNo ackCh (probeInterval conf)

  -- Send a ping to the node
  lift $ encodeAndSendMsg (addr node) (port node) ping

  -- Wait for response or round-trip-time.
  timeoutCh <- lift (timeAfter (probeTimeout conf))
  lift $ select2 ackCh timeoutCh $ \case
    -- OK
    C2a msg@(AckMessage True) ->
      putStrLn $ "msg: " ++ (show msg)

    C2a msg@(AckMessage False) ->
      putStrLn $ "not finish"

    -- Timeout
    C2b _ -> do
      putStrLn "timeout"

      -- Get some random live nodes.
      let kNodes = kRandomNodes

      -- Attempt an indirect ping.
      let indPingMsg = IndirectPingReq 0
      forM_ kNodes $ \node -> do
        sendMsg node indPingMsg

      -- Wait for the acks or timeout.
      select ackCh $ \msg -> do
        putStrLn $ "get msg finally:" ++ (show msg)
        if (complete msg) then do
          putStrLn "node fine"
          return ()
        else
          -- No acks received from target, suspect
          suspectNode node

probe :: Memberlist -> IO ()
probe memberlist = do
  let node = Node { name = "n1"
                  , addr = "127.0.0.1"
                  , port = 9010
                  }
  evalStateT (probeNode node) memberlist
  
nextSeqNo :: StateT Memberlist IO Int
nextSeqNo = do
  m <- get
  let newSeqNum = (sequenceNum m) + 1
  put $ m { sequenceNum = newSeqNum }
  return newSeqNum

gossip :: Memberlist -> IO ()
gossip memberlist = do
  let kNodes = kRandomNodes

  -- Attempt an indirect ping.
  let ping = Ping 0 "payload"
  forM_ kNodes $ \node -> do
    sendMsg node ping
