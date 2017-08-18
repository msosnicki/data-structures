module Problem1 where

import Data.List
import Data.Maybe
import Data.Functor
import Control.Monad
import System.IO
-- bracket matching

type Stack a = [a]
type Bracket = (Char, Int)

main :: IO ()
main =
  hSetBuffering stdin NoBuffering >>= \_ ->
  readWord >>= \word ->
  putStrLn $ solve word

opening = ['[', '(', '{']
closing = [']', ')', '}']
matches o c = isJust $ find ((o, c) == ) $ zip opening closing
matchesC = flip matches

solve word =
  let withIndex = zip word [1..]
      solution = foldl f (Right []) withIndex
  in case solution of
    Left i -> show i
    Right ((_, i):t) -> show i
    Right [] -> "Success"
  
f :: Either Int (Stack Bracket) -> (Char, Int) -> Either Int (Stack Bracket)
f acc@(Left i) _ = acc
f acc@(Right stack) ch@(char, i)
  | elem char opening = Right $ stackPush stack ch
  | elem char closing =
    let pop = stackPop stack
        validated = mfilter ((matchesC char) . fst . fst) pop
        next = case validated of
          Just(a, rest) -> Right rest
          Nothing -> Left i
    in next
  | otherwise = acc

stackPush :: Stack a -> a -> Stack a
stackPush l e = e : l

stackPop :: Stack a -> Maybe (a, Stack a)
stackPop [] = Nothing
stackPop (h:t) = Just (h, t)

readWord = getLine
