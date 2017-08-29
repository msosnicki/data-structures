module Problem2 where

import Control.Applicative
import Data.Char
import Data.Ord
import Data.List
import System.IO

-- building heap from list (constant space on array)
main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  n <- nextNum
  m <- nextNum
  nums <- nextNums m
  putStrLn "tte"

compareThread = comparing snd

solve n jobs =
  let pq = fromList compareThread $ zip [0..(n-1)] (repeat 0)
  in scanl processJob pq jobs

-- Heap related stuff
type Rank = Int
type Compare a = a -> a -> Ordering

data Heap a = Tip | Node !Rank a (Heap a) (Heap a) deriving Show

rank Tip = 0
rank (Node r _ _ _) = r

fromList :: Compare a -> [a] -> Heap a
fromList _ [] = Tip
fromList cmp (x:xs) = foldl' (insertHeap cmp) (singleton x) xs

singleton x = Node 1 x Tip Tip

insertHeap cmp h v = merge cmp h $ singleton v

merge :: Compare a -> Heap a -> Heap a -> Heap a
merge _ Tip h = h
merge _ h Tip = h
merge cmp h1@(Node _ x l1 r1) h2@(Node _ y l2 r2) =
  if cmp x y == GT then makeHeap x l1 (merge cmp r1 h2)
  else makeHeap y l2 (merge cmp h1 r2)

makeHeap x a b =
  if rank a >= rank b then Node (rank b + 1) x a b
  else Node (rank a + 1) x b a

peekMin Tip = Nothing
peekMin (Node _ x _ _) = x

extractMin _ Tip = Nothing
extractMin cmp (Node _ x l r) = Just (x, merge cmp l r)

-- IO related stuff
nextIOs io n = sequence $ replicate n io

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
