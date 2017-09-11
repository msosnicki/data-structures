module Problem2 where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import System.IO

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  m <- nextNum
  n <- nextNum
  commands <- nextIOs getLine n
  foldl' perform (return (emptyMap m)) (words <$> commands)
  return ()

perform :: IO (Map String) -> [String] -> IO (Map String)
perform actions ("add" : value : _) = (put value) <$> actions
perform actions ("del" : value : _) = (del value) <$> actions
perform actions ("find" : value : _) = do
  m <-actions
  putStrLn $ has value m
  return m
perform actions ("check" : i : _) = do
  m <- actions
  putStrLn $ check (read i) m
  return m
  

-- chained hash map

data Map e = Map {under :: S.Seq (S.Seq e), hash :: e -> Int}

emptyMap size = Map (S.fromList $ replicate size S.empty) (hashN size)

hashOf m k = (hash m) k

put v m = Map (S.adjust addOrUpdate (hashOf m v) (under m)) (hash m)
  where
    addOrUpdate s = fromMaybe (s S.|> v) $ (\i -> S.update i v s) <$> S.elemIndexL v s

del v m = Map (S.adjust delete (hashOf m v) (under m)) (hash m)
  where delete s = S.filter (/= v) s

has v m = F.foldl (\acc elem -> if(elem==v) then "yes" else acc) "no" seq
  where seq = S.index (under m) (hashOf m v)
-- hash

check i m = F.foldl (\acc elem -> acc ++ (show elem)) "" (S.index (under m) i)

p = 1000000007
x = 263

hashN m str = let
  zipped = map (\(i, ch) -> (ord ch) * (x^i)) (zip [0..] str)
  in ((`mod` m). (`mod` p)) (foldl' (+) 0 zipped) 

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
