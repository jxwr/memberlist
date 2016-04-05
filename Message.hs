module Message where

import Data.Word

type MsgType = Word8

pingMsgType = 0 :: MsgType
indirectPingMsgType = 1 :: MsgType
ackRespMsgType = 2 :: MsgType
suspectMsgType = 3 :: MsgType
aliveMsgType = 4 :: MsgType
deadMsgType = 5 :: MsgType
pushPullMsgType = 6 :: MsgType
compoundMsgType = 7 :: MsgType
userMsgType = 8 :: MsgType
compressMsgType = 9 :: MsgType
encryptMsgType = 10 :: MsgType
