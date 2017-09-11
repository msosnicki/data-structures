module Problem1 where

import Control.Applicative
import Data.Char
import qualified Data.Map as M
import Data.List
import Data.Maybe
import System.IO

-- hash map phone book

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  n <- nextNum
  ops <- nextOps n
  process ops
  return ()

process ops = sequence $ map io $ zip ops $ tail $ scanl f (M.empty) ops
  where f acc (Add num name) = M.insert num name acc 
        f acc (Del num) = M.delete num acc
        f acc _ = acc
        io ((Find num), acc) = putStrLn $ fromMaybe "not found" $ M.lookup num acc
        io _ = return ()
        
readInt :: String -> Int
readInt = read

nextOp :: IO MapOp
nextOp = do
  (op:t) <- words <$> getLine
  return $ case op of
    "add" -> Add (readInt $ t !! 0) (t !! 1)
    "find" -> Find (readInt $ t !! 0)
    "del" -> Del (readInt $ t !! 0)

nextOps = nextIOs nextOp

data MapOp = Add {num :: Int, name :: String} | Find {num :: Int} | Del {num :: Int} deriving Show

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
