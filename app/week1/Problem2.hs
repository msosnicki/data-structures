module Problem2 where

import Control.Applicative
import Control.Monad
import Data.Array
import Data.Char
import Data.List
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import System.IO
-- height of tree

data Tree a = Empty | Node a (Tree a) (Tree a)

main :: IO ()
main =
  hSetBuffering stdin NoBuffering >>= \_ ->
  nextNum >>= \n ->
  nextNums n >>= \nodes ->
  let t = tree nodes
  in putStrLn $ show $ depth t

-- depth :: Array Int [Int] -> Int
depth tree = depth' tree (-1)

-- depth' :: Array Int [Int] -> Int -> Int
depth' tree i =
  let children = tree ! i
  in if(S.null children) then 0 else (1+) $ F.maximum $ fmap (depth' tree) children

tree nodes =
  let size = length nodes - 1
      zipped = zip [0..size] nodes
      m (i, p) = [(i, S.empty), (p, S.singleton i)]
      joined = concatMap m zipped
  in accumArray (S.><) S.empty (-1, size) joined
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
