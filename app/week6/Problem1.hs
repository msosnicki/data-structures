module Problem1 where

import Control.Applicative
import Control.Monad.State
import Data.Char
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.Traversable as T
import qualified Data.List as L
import System.IO

import Debug.Trace

-- Traversals of a binary tree

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  n <- nextNum
  t <- readTree n
  let in' = traverseIn t
      pre = traversePre t
      post = traversePost t
      printOrd o = putStrLn $ L.intercalate " " $ show <$> o
  printOrd in'
  printOrd pre
  printOrd post

traverseIn Leaf = []
traverseIn (Node l k r) = (traverseIn l) ++ [k] ++ (traverseIn r)

traversePre Leaf = []
traversePre (Node l k r) = [k] ++ (traversePre l) ++ (traversePre r)

traversePost Leaf = []
traversePost (Node l k r) = (traversePost l) ++ (traversePost r) ++ [k]

-- tree
data Tree a = Leaf | Node {left :: Tree a, key :: a, right :: Tree a} deriving Show

type NodeInfo = (Int, Int, Int)

leftChild (l, _, _) = l
rightChild (_, _, r) = r

buildTree :: Int -> Seq.Seq NodeInfo -> Tree Int
buildTree (-1) nds = Leaf
buildTree root nds = Node (buildTree l nds) key (buildTree r nds)
  where (key, l, r) = Seq.index nds root

readTree :: Int -> IO (Tree Int)
readTree n = (\(n, root) -> buildTree root n) <$> withRoot
  where nodes = runStateT (nextIOs' readNodeS n) (Set.fromList [0..(n-1)])
        withRoot = (\(n, roots) -> (n, head $ Set.toList roots)) <$> nodes

readNodeS :: StateT (Set.Set Int) IO NodeInfo 
readNodeS = do
  roots <- get
  nd <- liftIO readNode
  put $ Set.difference roots (Set.fromList [leftChild nd, rightChild nd])
  return nd

io :: IO a -> StateT (Set.Set Int) IO a
io = liftIO

readNode = do
  k <- nextNum
  l <- nextNum
  r <- nextNum
  return (k, l, r)

-- IO related stuff
nextIOs' io n = T.sequence $ Seq.replicate n io
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
