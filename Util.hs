module Util where

import Struct
import BuggySelect

import Control.Concurrent

kRandomNodes = ["n1", "n2", "n3"]

second = 1000000

setTimeout ch n = go $ do
  threadDelay $ n * second
  writeChan ch Timeout

timeAfter n = do
  ch <- newChan
  setTimeout ch n
  return ch


