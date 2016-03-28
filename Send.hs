module Send where

import Struct
import Data.Bits
import Network.BSD
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (sendTo, recvFrom)
import Data.List
import Data.Aeson
import Data.ByteString.Lazy (unpack, toStrict, cons)
import qualified Data.ByteString as BS
import Control.Concurrent

import Message
import PingMsg

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
                  
                  case msgType of
                    pingMsgType -> 
                      case decodeStrict (BS.tail bytes) :: Maybe PingMsg of
                        Just ping -> putStrLn (show ping)
                        Nothing -> putStr "nothing"
                  procMessages sock
              
