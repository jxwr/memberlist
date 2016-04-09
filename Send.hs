module Send where

import Struct
import Data.Bits
import Network.BSD
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (sendTo, recvFrom)
import Data.List
import Data.Aeson
import Data.Text (unpack, pack, empty, Text)
import Data.ByteString.Lazy (toStrict, cons)
import qualified Data.ByteString as BS
import Control.Concurrent
import qualified Data.Map as Map

import Message
import PingMsg
import IndirectPingMsg
import AckRespMsg
import SuspectMsg
import DeadMsg
import AliveMsg


encodeAndSendMsg :: ToJSON msg => String -> Int -> MsgType -> msg -> IO ()
encodeAndSendMsg destAddr port msgType msg = do
  
  addrinfos <- getAddrInfo Nothing (Just destAddr) (Just (show port))
  
  let serverAddr = head addrinfos

  sock <- socket (addrFamily serverAddr) Datagram defaultProtocol  

  encodeAndSendMsgToAddr sock (addrAddress serverAddr) msgType msg

encodeAndSendMsgOverSock :: ToJSON msg => Socket -> String -> Int -> MsgType -> msg -> IO ()
encodeAndSendMsgOverSock sock destAddr port msgType msg = do
  
  addrinfos <- getAddrInfo Nothing (Just destAddr) (Just (show port))
  
  let serverAddr = head addrinfos

  encodeAndSendMsgToAddr sock (addrAddress serverAddr) msgType msg  
  
encodeAndSendMsgToAddr :: ToJSON msg => Socket -> SockAddr -> MsgType -> msg -> IO ()
encodeAndSendMsgToAddr sock destAddr msgType msg = do
  
  let bytes = toStrict (cons msgType $ encode msg)
  
  n <- sendTo sock bytes destAddr
  
  putStrLn $ "Sent " ++ show n ++ " bytes to " ++
    show destAddr ++ " => " ++ show bytes

udpListen :: MVar (Map.Map Int AckHandler2) -> Int -> IO ()
udpListen handlerMap port = withSocketsDo $ do
  do addrinfos <- getAddrInfo 
       (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
       Nothing (Just (show port))
       
     let serveraddr = head addrinfos
     
     sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
     
     bindSocket sock (addrAddress serveraddr)

     procMessages sock
       where procMessages sock =
               do putStrLn "========= proc messages ========="
                  
                  (bytes, addr) <- recvFrom sock 1024
                  
                  let msgType = BS.head bytes
                      rawMsg = BS.tail bytes

                  putStrLn $ "[DEBUG] * type=" ++ show msgType
                  
                  handleCommand msgType rawMsg (sock, addr)
                  
                  procMessages sock
                  
             handleCommand msgType rawMsg from
               | msgType == pingMsgType = 
                   case decodeStrict rawMsg :: Maybe PingMsg of
                     Just msg -> handlePing msg from handlerMap
                     Nothing -> return ()
                     
               | msgType == indirectPingMsgType =
                   case decodeStrict rawMsg :: Maybe IndirectPingMsg of
                     Just msg -> handleIndirectPing msg from handlerMap
                     Nothing -> return ()
                     
               | msgType == ackRespMsgType =
                   case decodeStrict rawMsg :: Maybe AckRespMsg of
                     Just msg -> handleAck msg from handlerMap
                     Nothing -> return ()
                     
               | msgType == suspectMsgType =
                   case decodeStrict rawMsg :: Maybe SuspectMsg of
                     Just msg -> handleSuspect msg from handlerMap
                     Nothing -> return ()
                     
               | msgType == aliveMsgType =
                   case decodeStrict rawMsg :: Maybe AliveMsg of
                     Just msg -> handleAlive msg from handlerMap
                     Nothing -> return ()
                     
               | msgType == deadMsgType =
                   case decodeStrict rawMsg :: Maybe DeadMsg of
                     Just msg -> handleDead msg from handlerMap
                     Nothing -> return ()

handlePing msg (sock, fromAddr) handlerMap = do
  let ackMsg = AckRespMsg (PingMsg.seqNo msg) empty
  encodeAndSendMsgToAddr sock fromAddr ackRespMsgType ackMsg

handleIndirectPing msg (sock, _) handlerMap = do
  let IndirectPingMsg seqNo target port node = msg
      pingMsg = PingMsg 99 node
  encodeAndSendMsgOverSock sock (unpack target) port pingMsgType pingMsg

handleAck msg from handlerMap = do
  putStrLn $ "ACK:" ++ (show msg)
  invokeAckHandler handlerMap msg 

handleSuspect msg from handlerMap = putStrLn (show msg)

handleAlive msg from handlerMap = putStrLn (show msg)

handleDead msg from handlerMap = putStrLn (show msg)



----------------------------------------------------------
------------------- AckHandler ---------------------------
----------------------------------------------------------

type AckHandlerFunc = Text -> Int -> IO ()
data AckHandler2 = AckHandler2 AckHandlerFunc 
newHandlerMap :: IO (MVar (Map.Map Int AckHandler2))
newHandlerMap = newMVar Map.empty

setAckHandler handlerMap seq handler = do
  handlers <- takeMVar handlerMap
  putMVar handlerMap $ Map.insert seq handler handlers

-- ackHanlders用处有两个：
-- 1. 通过setAckChannel，阻塞的方式得到AckMessage（与AckRespMsg不同，是一个封装）
-- 2. 通过setAckHandler，直接设置一个自定义的handler
invokeAckHandler handlerMap (AckRespMsg seq payload) = do
  handlers <- takeMVar handlerMap
  case Map.lookup seq handlers of
    Just (AckHandler2 func) -> do
      putMVar handlerMap (Map.delete seq handlers)
      func payload 0
    Nothing -> putMVar handlerMap handlers

---------------------------------------------
------------------- Test --------------------
---------------------------------------------

testUdp port = do
  handlerMap <- newHandlerMap
  setAckHandler handlerMap 99 $ AckHandler2 (\payload time -> putStrLn "===> Call Ack Handler")
  udpListen handlerMap port

testSend typ str
  | typ == 0 =
    let msg = PingMsg 1 (pack str)
    in encodeAndSendMsg "127.0.0.1" 8910 typ msg
  | typ == 1 =
    let msg = IndirectPingMsg 1 (pack str) 9000 (pack str)
    in encodeAndSendMsg "127.0.0.1" 8910 typ msg
  | typ == 2 =
    let msg = AckRespMsg 1 (pack str)
    in encodeAndSendMsg "127.0.0.1" 8910 typ msg
  | typ == 3 =
    let msg = SuspectMsg 1 (pack str) (pack str)
    in encodeAndSendMsg "127.0.0.1" 8910 typ msg
  | typ == 4 =
    let msg = AliveMsg 1 (pack "127.0.0.1") (pack str) 9000 (pack str)
    in encodeAndSendMsg "127.0.0.1" 8910 typ msg
  | typ == 5 =
    let msg = DeadMsg 1 (pack str) (pack str) 
    in encodeAndSendMsg "127.0.0.1" 8910 typ msg       
