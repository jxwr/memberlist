module Net where

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

---

type HandlerFunc = Socket -> SockAddr -> String -> IO ()

serveLog :: String              -- ^ Port number or name; 514 is default
         -> HandlerFunc         -- ^ Function to handle incoming messages
         -> IO ()
serveLog port handlerfunc = withSocketsDo $
  do addrinfos <- getAddrInfo 
       (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
       Nothing (Just port)
     let serveraddr = head addrinfos
     sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
     bindSocket sock (addrAddress serveraddr)
     procMessages sock
       where procMessages sock =
               do (msg, n, addr) <- recvFrom sock 1024
                  handlerfunc sock addr msg
                  procMessages sock

-- A simple handler that prints incoming packets
plainHandler :: HandlerFunc
plainHandler sock addr msg = do
    putStrLn $ "From " ++ (show addr) ++ ": " ++ msg
    sendTo sock ("pong:" ++ msg) addr
    return ()
