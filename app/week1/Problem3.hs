module Problem3 where

import Control.Applicative
import Data.Char
import Data.List
import System.IO

-- network simulation

main :: IO ()
main =
  hSetBuffering stdin NoBuffering >>= \_ ->
  putStrLn "tete"

-- Queue

data Queue a = Queue {enqueue :: [a], dequeue :: [a], size :: Int}

emptyQueue = Queue [] [] 0

push :: Queue a -> a -> Queue a
push q a = Queue (a : enqueue q) (dequeue q) (size q + 1)

pop :: Queue a -> (Maybe a, Queue a)
pop q =
  case (dequeue q, enqueue q) of
    ([], []) -> (Nothing, emptyQueue)
    ((h:t), _) -> (Just h, Queue (enqueue q) t (size q - 1))
    ([], e) -> pop $ Queue [] (reverse e) (size q)

-- IO related stuff

nextNums n = sequence $ replicate n nextNum

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
