{-# LANGUAGE TupleSections #-}
module Problem1 where

import Control.Applicative
import Control.Monad.ST
import Data.STRef
import Data.Array.ST
import Data.Char
import Data.Maybe
import Data.List
import System.IO

-- building heap from list (constant space on array)

cmp = flip compare

main :: IO ()
main =
  hSetBuffering stdin NoBuffering >>= \_ ->
  nextNum >>= \n ->
  nextNums n >>= \nums ->
  let result = stToIO $ buildTree nums
  in result >>= (putStrLn . show)

buildTree :: [Int] -> ST s (Int, [Int])
buildTree nums =
  let size = length nums
      array = newListArray (1, size) nums :: ST s (STArray s Int Int)
      swaps = newSTRef 0
      upTo = size `div` 2
      toFix = [upTo, upTo - 1 .. 1]
      fixing a s = sequence $ (\i -> siftDown a i s) <$> toFix
  in
    do a <- array
       s <- swaps
       fixing a s
       (,) <$> readSTRef s <*> getElems a

listToArray :: [a] -> ST s (STArray s Int a)
listToArray l = newListArray (1, length l) l

siftDown arr i count =
  readArray arr i >>= \element ->
  children arr i >>= \ch ->
  case ch of
    [] -> return ()
    _ ->
      let (chI, chV) = maxBy snd ch
          isBroken = cmp element chV == LT
      in if(isBroken) then swap arr i chI count >> siftDown arr chI count else return ()

maxBy extract = maximumBy (\a b -> cmp (extract a) (extract b))

  -- let isMax = (cmp toFix (maximumBy cmp [l,r])) /= GT
  --     swapWith = if(isMax) then Just $ if(cmp l r == GT) then left i else right i else Nothing
  --     swapOp j = swap arr i j count
  -- in
  --   fromMaybe (return ()) $ swapOp <$> swapWith

children arr i =
  getBounds arr >>= \b ->
  let try ix = if(ix > snd b) then return [] else (\iv -> [(ix,iv)]) <$> readArray arr ix
      tl = try $ left i
      tr = try $ right i
  in (++) <$> tl <*> tr

swap arr i j c =
  readArray arr i >>= \iv ->
  readArray arr j >>= \jv ->
  readSTRef c >>= \count ->
  writeArray arr i jv >> writeArray arr j iv >> writeSTRef c (count + 1)

left i = 2*i
right i = 2*i + 1
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
