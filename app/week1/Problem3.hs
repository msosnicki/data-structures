module Problem3 where

import Control.Applicative
import Data.Char
import Data.Maybe
import Data.List
import System.IO

-- network simulation

newtype Packet = Packet (Int, Int) deriving (Show)

main :: IO ()
main =
  hSetBuffering stdin NoBuffering >>= \_ ->
  nextNum >>= \buffer ->
  nextNum >>= \n ->
  nextPackets n >>= \packets ->
  putStrLn $ show $ packets 

start :: Packet -> Int
start (Packet (s, _)) = s

duration :: Packet -> Int
duration (Packet (_, d)) = d

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
