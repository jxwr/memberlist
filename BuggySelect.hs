module BuggySelect where

import Control.Concurrent
import Control.Exception

go = forkIO

data C2 a b = C2a a | C2b b
data C3 a b c = C3a a | C3b b | C3c c
data C4 a b c d = C4a a | C4b b | C4c c | C4d d

selectIO :: Chan a -> IO a
selectIO c = do
  a <- readChan c
  return a

select2IO :: Chan a -> Chan b -> IO (C2 a b)
select2IO c1 c2 = do
  ch <- newChan
  pid1 <- go $ do { a <- readChan c1; writeChan ch (C2a a)}
  pid2 <- go $ do { b <- readChan c2; writeChan ch (C2b b)}
  t2 <- readChan ch
  throwTo pid1 ThreadKilled
  throwTo pid2 ThreadKilled
  return t2

select3IO :: Chan a -> Chan b -> Chan c -> IO (C3 a b c)
select3IO c1 c2 c3 = do
  ch <- newChan
  pid1 <- go $ do { a <- readChan c1; writeChan ch (C3a a)}
  pid2 <- go $ do { b <- readChan c2; writeChan ch (C3b b)}
  pid3 <- go $ do { c <- readChan c3; writeChan ch (C3c c)}
  t3 <- readChan ch
  throwTo pid1 ThreadKilled
  throwTo pid2 ThreadKilled
  throwTo pid3 ThreadKilled
  return t3

select4IO :: Chan a -> Chan b -> Chan c -> Chan d -> IO (C4 a b c d)
select4IO c1 c2 c3 c4 = do
  ch <- newChan
  pid1 <- go $ do { a <- readChan c1; writeChan ch (C4a a)}
  pid2 <- go $ do { b <- readChan c2; writeChan ch (C4b b)}
  pid3 <- go $ do { c <- readChan c3; writeChan ch (C4c c)}
  pid4 <- go $ do { d <- readChan c4; writeChan ch (C4d d)}
  t4 <- readChan ch
  throwTo pid1 ThreadKilled
  throwTo pid2 ThreadKilled
  throwTo pid3 ThreadKilled
  throwTo pid4 ThreadKilled
  return t4

select c1 f = do { r <- selectIO c1; f r }
select2 c1 c2 f = do { r <- select2IO c1 c2; f r }
select3 c1 c2 c3 f = do { r <- select3IO c1 c2 c3; f r }
select4 c1 c2 c3 c4 f = do { r <- select4IO c1 c2 c3 c4; f r }
