module Send where

import Struct
import Data.Bits
import Network.BSD
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (sendTo, recvFrom)
import Data.List
import Data.Aeson
import Data.Text (pack)
import Data.ByteString.Lazy (unpack, toStrict, cons)
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
  
  let bytes = toStrict (cons msgType $ encode msg)
  
  n <- sendTo sock bytes (addrAddress serverAddr)
  
  putStrLn $ "sent " ++ show n ++ " bytes: " ++ (show bytes)

udpListen :: Int -> IO ()
udpListen port = withSocketsDo $ do
  do addrinfos <- getAddrInfo 
       (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
       Nothing (Just (show port))
       
     let serveraddr = head addrinfos
     putStrLn (show serveraddr)
     
     sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
     
     bindSocket sock (addrAddress serveraddr)
     
     procMessages sock
       where procMessages sock =
               do (bytes, addr) <- recvFrom sock 1024
                  let msgType = BS.head bytes
                      rawMsg = BS.tail bytes

                  putStrLn $ "DEBUG * type=" ++ show msgType

                  handleCommand msgType rawMsg
                  
                  procMessages sock
                  
             handleCommand msgType rawMsg
               | msgType == pingMsgType = 
                   case decodeStrict rawMsg :: Maybe PingMsg of
                     Just ping -> putStrLn (show ping)
                     Nothing -> putStr "nothing"
               | msgType == indirectPingMsgType =
                   case decodeStrict rawMsg :: Maybe IndirectPingMsg of
                     Just ping -> putStrLn (show ping)
                     Nothing -> putStr "nothing"
               | msgType == ackRespMsgType =
                   case decodeStrict rawMsg :: Maybe AckRespMsg of
                     Just ping -> putStrLn (show ping)
                     Nothing -> putStr "nothing"
               | msgType == suspectMsgType =
                   case decodeStrict rawMsg :: Maybe SuspectMsg of
                     Just ping -> putStrLn (show ping)
                     Nothing -> putStr "nothing"
               | msgType == aliveMsgType =
                   case decodeStrict rawMsg :: Maybe AliveMsg of
                     Just ping -> putStrLn (show ping)
                     Nothing -> putStr "nothing"
               | msgType == deadMsgType =
                   case decodeStrict rawMsg :: Maybe DeadMsg of
                     Just ping -> putStrLn (show ping)
                     Nothing -> putStr "nothing"
