module Net where

import Struct
import Data.Bits
import Network.Socket
import Network.BSD
import Data.List
import Control.Concurrent


encodeAndSendMsg destAddr port msg = do
  addrinfos <- getAddrInfo Nothing (Just destAddr) (Just (show port))
  let serverAddr = head addrinfos
  sock <- socket (addrFamily serverAddr) Datagram defaultProtocol
  n <- sendTo sock (show msg) (addrAddress serverAddr)
  putStrLn $ "sent " ++ show n ++ " bytes: " ++ (show msg)


udpListen :: Memberlist -> String -> IO ()
udpListen m port = withSocketsDo $ do
  do addrinfos <- getAddrInfo 
       (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
       Nothing (Just port)
     let serveraddr = head addrinfos
     sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
     bindSocket sock (addrAddress serveraddr)
     procMessages sock
       where procMessages sock =
               do (msg, n, addr) <- recvFrom sock 1024
                  handleCommand m sock addr msg
                  procMessages sock


type HandlerFunc = Memberlist -> Socket -> SockAddr -> String -> IO ()

handleCommand :: HandlerFunc
handleCommand m sock addr msg = do
    putStrLn $ "From " ++ (show addr) ++ ": " ++ msg
    sendTo sock ("pong:" ++ msg) addr
    return ()
