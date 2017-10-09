module Problem1 where

import Control.Applicative
import Data.Char
import qualified Data.Foldable as F
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.Traversable as T
import qualified Data.List as L
import System.IO

-- Traversals of a binary tree
type Print = IO ()

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  n <- nextNum
  nodes <- readNodes n
  traverseIn nodes
  traversePre nodes
  traversePost nodes

traverseTree f nodes = loop 0 >> putStr "\n"
  where loop i = 
          let (k, l, r) = Seq.index nodes i
              left = if(l == -1) then return () else loop l
              right = if(r == -1) then return () else loop r
              key = putStr $ concat [show k, " "]
          in (f left key right)

traverseIn = traverseTree (\left key right -> left >> key >> right)

traversePre = traverseTree (\left key right -> key >> left >> right)

traversePost = traverseTree (\left key right -> left >> right >> key)

type NodeInfo = (Int, Int, Int)

readNodes = nextIOs readNode

readNode = do
  k <- nextNum
  l <- nextNum
  r <- nextNum
  return (k, l, r)

-- IO related stuff
nextIOs io n = Seq.replicateM n io

nextNums n = (F.toList) <$> (nextIOs nextNum n)
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
