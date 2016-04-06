module Send where

import Struct
import Data.Bits
import Network.BSD
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (sendTo, recvFrom)
import Data.List
import Data.Aeson
import Data.Text (unpack, pack, empty)
import Data.ByteString.Lazy (toStrict, cons)
import qualified Data.ByteString as BS
import Control.Concurrent

import Message
import PingMsg
import IndirectPingMsg
import AckRespMsg
import SuspectMsg
import DeadMsg
import AliveMsg

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

encodeAndSendMsg :: ToJSON msg => String -> Int -> MsgType -> msg -> IO ()
encodeAndSendMsg destAddr port msgType msg = do
  
  addrinfos <- getAddrInfo Nothing (Just destAddr) (Just (show port))
  
  let serverAddr = head addrinfos

  sock <- socket (addrFamily serverAddr) Datagram defaultProtocol  

  encodeAndSendMsgToAddr sock (addrAddress serverAddr) msgType msg
  
encodeAndSendMsgToAddr :: ToJSON msg => Socket -> SockAddr -> MsgType -> msg -> IO ()
encodeAndSendMsgToAddr sock destAddr msgType msg = do
  
  let bytes = toStrict (cons msgType $ encode msg)
  
  n <- sendTo sock bytes destAddr
  
  putStrLn $ "Sent " ++ show n ++ " bytes to " ++
    show destAddr ++ " => " ++ show bytes

udpListen :: Int -> IO ()
udpListen port = withSocketsDo $ do
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
                     Just msg -> handlePing msg from
                     Nothing -> return ()
                     
               | msgType == indirectPingMsgType =
                   case decodeStrict rawMsg :: Maybe IndirectPingMsg of
                     Just msg -> handleIndirectPing msg from
                     Nothing -> return ()
                     
               | msgType == ackRespMsgType =
                   case decodeStrict rawMsg :: Maybe AckRespMsg of
                     Just msg -> handleAck msg from
                     Nothing -> return ()
                     
               | msgType == suspectMsgType =
                   case decodeStrict rawMsg :: Maybe SuspectMsg of
                     Just msg -> handleSuspect msg from
                     Nothing -> return ()
                     
               | msgType == aliveMsgType =
                   case decodeStrict rawMsg :: Maybe AliveMsg of
                     Just msg -> handleAlive msg from
                     Nothing -> return ()
                     
               | msgType == deadMsgType =
                   case decodeStrict rawMsg :: Maybe DeadMsg of
                     Just msg -> handleDead msg from
                     Nothing -> return ()

handlePing msg (sock, fromAddr) = do
  let ackMsg = AckRespMsg (PingMsg.seqNo msg) empty
  encodeAndSendMsgToAddr sock fromAddr ackRespMsgType ackMsg

handleIndirectPing msg (sock, _) = do
  let IndirectPingMsg seqNo target port node = msg
      pingMsg = PingMsg 99 node
  addrinfos <- getAddrInfo Nothing (Just (unpack target)) (Just (show port))
  let serverAddr = head addrinfos
  encodeAndSendMsgToAddr sock (addrAddress serverAddr) pingMsgType pingMsg

handleAck msg from =
  putStrLn $ "ACK:" ++ (show msg)

handleSuspect msg from = putStrLn (show msg)

handleAlive msg from = putStrLn (show msg)

handleDead msg from = putStrLn (show msg)
