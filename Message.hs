module Message where

import Data.Word

type MsgType = Word8

pingMsgType = 0 :: MsgType
indirectPingMsg = 1 :: MsgType
ackRespMsg = 2 :: MsgType
suspectMsg = 3 :: MsgType
aliveMsg = 4 :: MsgType
deadMsg = 5 :: MsgType
pushPullMsg = 6 :: MsgType
compoundMsg = 7 :: MsgType
userMsg = 8 :: MsgType
compressMsg = 9 :: MsgType
encryptMsg = 10 :: MsgType
