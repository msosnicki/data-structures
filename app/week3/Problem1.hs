{-# LANGUAGE TupleSections #-}
module Problem1 where

import Control.Applicative
import Control.Monad.ST
import Data.STRef
import Data.Foldable (for_)
import Data.Array.ST
import Data.Char
import Data.Maybe
import Data.List
import System.IO

-- building heap from list (constant space on array)

type Acc = (Int, [(Int, Int)])

cmp = flip compare

main :: IO ()
main =
  hSetBuffering stdin NoBuffering >>= \_ ->
  nextNum >>= \n ->
  nextNums n >>= \nums ->
  let result = stToIO $ buildTree nums
      printTuple = \(a, b) -> putStrLn $ concat [show a, " ", show b]
  in result >>= \((swapNo, swaps), _) ->
    putStrLn (show swapNo) >> for_ (reverse swaps) printTuple

buildTree :: [Int] -> ST s (Acc, [Int])
buildTree nums =
  let size = length nums
      array = newListArray (1, size) nums :: ST s (STArray s Int Int)
      accum = newSTRef (0, [])
      upTo = size `div` 2
      toFix = [upTo, upTo - 1 .. 1]
      fixing arr acc = sequence $ (\i -> siftDown arr i acc) <$> toFix
  in
    do arr <- array
       acc <- accum
       fixing arr acc
       (,) <$> readSTRef acc <*> getElems arr

siftDown arr i acc =
  readArray arr i >>= \element ->
  children arr i >>= \ch ->
  case ch of
    [] -> return ()
    _ ->
      let (chI, chV) = maxBy snd ch
          isBroken = cmp element chV == LT
      in if(isBroken) then swap arr i chI acc >> siftDown arr chI acc else return ()

maxBy extract = maximumBy (\a b -> cmp (extract a) (extract b))

children arr i =
  getBounds arr >>= \b ->
  let try ix = if(ix > snd b) then return [] else (\iv -> [(ix,iv)]) <$> readArray arr ix
      tl = try $ left i
      tr = try $ right i
  in (++) <$> tl <*> tr

swap arr i j c =
  readArray arr i >>= \iv ->
  readArray arr j >>= \jv ->
  readSTRef c >>= \acc ->
  let (count, swaps) = acc
  in writeArray arr i jv >> writeArray arr j iv >> writeSTRef c (count + 1, (i - 1, j - 1) : swaps)

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
