module Problem3 where

import Control.Applicative
import Data.Char
import Data.Maybe
import Data.List
import System.IO

-- network simulation

main :: IO ()
main =
  hSetBuffering stdin NoBuffering >>= \_ ->
  nextNum >>= \buffer ->
  nextNum >>= \n ->
  nextPackets n >>= \packets ->
  let solved = map start $ solve packets buffer
      report = sequence $ map (putStrLn . show) solved
  in () <$ report 

solve packets buffer =
  let scanned = scanl (f buffer) emptyAcc packets
  in tail scanned

f buffer (Acc (queue, lastFinish, start)) p@(Packet (arrived, duration)) =
  let queueAtT = dropWhileQ queue ((<=arrived) . end)
      nextStart = max lastFinish arrived
      p' = Packet (nextStart, nextStart + duration)
  in
    if(size queueAtT >= buffer)
    then Acc (queueAtT, lastFinish, -1)
    else Acc (push queueAtT p', nextStart + duration, nextStart)

-- Utils
newtype Packet = Packet (Int, Int) deriving (Show)
arrival :: Packet -> Int
arrival (Packet(a, _)) = a
end :: Packet -> Int
end (Packet(_, e)) = e

newtype Acc = Acc (Queue Packet, Int, Int) deriving (Show)
start :: Acc -> Int
start (Acc(_, _, s)) = s
emptyAcc = Acc (emptyQueue, 0, 0)
-- Queue

data Queue a = Queue {enqueue :: [a], dequeue :: [a], size :: Int} deriving (Show)

emptyQueue = Queue [] [] 0

push :: Queue a -> a -> Queue a
push q a = Queue (a : enqueue q) (dequeue q) (size q + 1)

pop :: Queue a -> (Maybe a, Queue a)
pop q =
  case (dequeue q, enqueue q) of
    ([], []) -> (Nothing, emptyQueue)
    ((h:t), _) -> (Just h, Queue (enqueue q) t (size q - 1))
    ([], e) -> pop $ Queue [] (reverse e) (size q)

dropWhileQ q p =
  let (popped, rem) = pop q
      next = popped >>= \e -> if(p e) then Just $ dropWhileQ rem p else Nothing
  in fromMaybe q next


-- IO related stuff
nextIOs io n = sequence $ replicate n io

nextPacket =
  nextNum >>= \start ->
  nextNum >>= \duration ->
  pure $ Packet (start, duration)

nextPackets = nextIOs nextPacket

nextNums = nextIOs nextNum

nextNum = nextNum' ""

nextNum' :: (Integral a, Read a) => String -> IO a
nextNum' acc =
  getChar >>= \ch ->
  case (ch, acc) of
    ('-', []) -> nextNum' $ ch : acc
    (char, acc) ->
      if(isDigit char) then nextNum' $ char : acc
      else if(null acc) then nextNum' ""
      else pure $ read $ reverse acc
