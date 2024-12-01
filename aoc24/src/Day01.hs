{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Day01 (solve) where

import Common.Util (Solution, countElem, zipWith')
import Data.List (nub, sort, transpose)

solve :: (Solution, Solution)
solve = (solve1, solve2)

example :: String
example =
  "\
  \3   4\n\
  \4   3\n\
  \2   5\n\
  \1   3\n\
  \3   9\n\
  \3   3\
  \"

parse :: String -> [[Int]]
parse =
  transpose
    . map (map read . words)
    . lines

-- >>> solve1 example
-- 11
solve1 :: Solution
solve1 =
  sum
    . map abs
    . zipWith' (-)
    . map sort
    . parse

-- >>> solve2 example
-- 13
solve2 :: Solution
solve2 input =
  sum $
    zipWith (*) (nub l) $
      map (`countElem` r) (nub l)
 where
  [l, r] = parse input
