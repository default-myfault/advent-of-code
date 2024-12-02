{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Day02 (solve) where

import Common.Util (Solution, allEq, countElem)
import Data.List (permutations)

solve :: (Solution, Solution)
solve = (solve1, solve2)

example :: String
example =
  "\
  \7 6 4 2 1\n\
  \1 2 7 8 9\n\
  \9 7 6 2 1\n\
  \1 3 2 4 5\n\
  \8 6 4 4 1\n\
  \1 3 6 7 9\n\
  \"

parse :: String -> [[Int]]
parse = map (map read . words) . lines

safe :: [Int] -> Bool
safe xs = monotone && inRange
 where
  deltas = zipWith (-) xs (tail xs)

  monotone = all (> 0) deltas || all (< 0) deltas
  inRange = all ((<= 3) . abs) deltas

-- >>> map (\xs -> (filter (\ls -> length ls == length xs) $ permutations xs)) (parse example)

-- >>> solve1 example
-- 2

solve1 :: Solution
solve1 = length . filter safe . parse

-- >>> solve2 example
-- 4

solve2 :: Solution
solve2 = length . filter (any safe) . map missingOne . parse
 where
  missingOne xs = [remove i xs | i <- [0 .. length xs - 1]]

  remove 0 (_ : xs) = xs
  remove n (x : xs) = x : remove (n - 1) xs
