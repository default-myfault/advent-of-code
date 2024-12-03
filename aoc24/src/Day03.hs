{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Day03 (solve) where

import Common.Util (Solution)
import Control.Applicative
import Data.List (elemIndex, stripPrefix)
import Text.Read (readMaybe)

solve :: (Solution, Solution)
solve = (solve1, solve2)

example1 :: String
example1 = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

example2 :: String
example2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

data Mult = Mult (Int, Int) | Dont | Do

parse :: String -> [Mult]
parse [] = []
parse xs = case parsed of
  Nothing -> parse (tail xs)
  Just (v, rest) -> v : parse rest
 where
  parsed :: Maybe (Mult, String)
  parsed = do
    let dont = fmap (Dont,) (stripPrefix "don't()" xs)
    let do' = fmap (Do,) (stripPrefix "do()" xs)

    dont <|> do' <|> do
      xs' <- stripPrefix "mul" xs
      closing <- elemIndex ')' xs'
      let (args, rest) = splitAt (closing + 1) xs' :: (String, String)
      val <- readMaybe args :: Maybe (Int, Int)

      Just (Mult val, rest)

-- >>> solve1 example1
-- 161
solve1 :: Solution
solve1 = sum . map mult . parse
 where
  mult (Mult (a, b)) = a * b
  mult _ = 0

-- >>> solve2 example2
-- 48
solve2 :: Solution
solve2 = sum . eval True . parse
 where
  eval _ [] = []
  eval on (x : xs) = case x of
    Do -> eval True xs
    Dont -> eval False xs
    Mult (a, b) -> if on then a * b : eval on xs else eval on xs
