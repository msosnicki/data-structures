{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Problem2 where

import Control.Applicative
import Data.Char
import Data.Foldable(for_)
import Data.Ord
import Data.List
import System.IO

type ThreadTime = (Int, Int)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  n <- nextNum
  m <- nextNum
  nums <- nextNums m
  let result = solve n nums
      printTuple = \(a, b) -> putStrLn $ concat [show a, " ", show b]
  for_ result printTuple

instance (Eq (Elem (Int, Int))) where
  (Elem (_, t1)) == (Elem (_, t2)) = t1 == t2

instance Ord (Elem (Int, Int)) where
  compare (Elem (i1, t1)) (Elem (i2, t2))
    | (flip compare) t1 t2 == EQ = (flip compare) i1 i2
    | otherwise = (flip compare) t1 t2

solve :: Int -> [Int] -> [ThreadTime]
solve n jobs =
  let tuples :: [ThreadTime] = zip [0..(n-1)] (repeat 0)
      pq = fromList $ map Elem tuples
      results = map fst $ scanl processJob ((0,0), pq) jobs
  in tail results 

processJob (_, pq) dur =
  let Just (Elem (threadNo, start), pq') = extractMin pq
  in ((threadNo, start), insertHeap pq' (Elem (threadNo, start + dur)))

-- Heap related stuff
type Rank = Int

data Elem a = Elem a deriving Show

data Heap a = Tip | Node !Rank (Elem a) (Heap a) (Heap a) deriving Show

rank Tip = 0
rank (Node r _ _ _) = r

fromList :: Ord (Elem a) => [Elem a] -> Heap a
fromList [] = Tip
fromList (x:xs) = foldl' insertHeap (singleton x) xs

singleton x = Node 1 x Tip Tip

insertHeap h v = merge h $ singleton v

merge :: Ord (Elem a) => Heap a -> Heap a -> Heap a
merge Tip h = h
merge h Tip = h
merge h1@(Node _ x l1 r1) h2@(Node _ y l2 r2) =
  if compare x y == GT then makeHeap x l1 (merge r1 h2)
  else makeHeap y l2 (merge h1 r2)

makeHeap x a b =
  if rank a >= rank b then Node (rank b + 1) x a b
  else Node (rank a + 1) x b a

peekMin Tip = Nothing
peekMin (Node _ x _ _) = Just x

extractMin Tip = Nothing
extractMin (Node _ x l r) = Just (x, merge l r)

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
