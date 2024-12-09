module Day07 (solve) where

import Common.Util (Solution, splitOn)
import Control.Monad (replicateM)

solve :: (Solution, Solution)
solve = (solve1, solve2)

example :: String
example =
  "190: 10 19\n\
  \3267: 81 40 27\n\
  \83: 17 5\n\
  \156: 15 6\n\
  \7290: 6 8 6 15\n\
  \161011: 16 10 13\n\
  \192: 17 8 14\n\
  \21037: 9 7 18 13\n\
  \292: 11 6 16 20"

parse :: String -> [(Int, [Int])]
parse = map parseLine . lines
 where
  parseLine l =
    let (r, xs) = splitOn ':' l
     in (read r, map read $ words xs)

hasSolution :: (Eq a) => [a -> a -> a] -> (a, [a]) -> Bool
hasSolution operators (r, ls) = elem r $ map (go ls) $ replicateM (length ls) operators
 where
  go [x] _ = x
  go (x : y : xs) (f : fs) = go (f x y : xs) fs
  go _ _ = undefined

-- >>> solve1 example
-- 3749
solve1 :: Solution
solve1 = sum . map fst . filter (hasSolution [(+), (*)]) . parse

-- >>> solve2 example
-- 11387
solve2 :: Solution
solve2 = sum . map fst . filter (hasSolution [(+), (*), (+|)]) . parse
 where
  a +| b = a * (10 ^ length (show b)) + b
