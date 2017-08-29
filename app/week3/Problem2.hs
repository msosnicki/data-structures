module Problem2 where

import Control.Applicative
import Data.Char
import Data.List
import System.IO

-- building heap from list (constant space on array)
main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  putStrLn "tete"

-- Heap related stuff
type Rank = Int

data Heap a = Tip | Node !Rank a (Heap a) (Heap a) deriving Show

rank Tip = 0
rank (Node r _ _ _) = r

fromList :: Ord a => [a] -> Heap a
fromList [] = Tip
fromList (x:xs) = foldl' insertHeap (singleton x) xs

singleton x = Node 1 x Tip Tip

insertHeap h v = merge h $ singleton v

merge :: Ord a => Heap a -> Heap a -> Heap a
merge Tip h = h
merge h Tip = h
merge h1@(Node _ x l1 r1) h2@(Node _ y l2 r2) =
  if x<=y then makeHeap x l1 (merge r1 h2)
  else makeHeap y l2 (merge h1 r2)

makeHeap x a b =
  if rank a >= rank b then Node (rank b + 1) x a b
  else Node (rank a + 1) x b a

peekMin Tip = Nothing
peekMin (Node _ x _ _) = x

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
