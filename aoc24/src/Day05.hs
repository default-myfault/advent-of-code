{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Day05 (solve) where

import Common.Util (Solution, splitOn)
import Data.List (sortBy)

solve :: (Solution, Solution)
solve = (solve1, solve2)

example :: String
example =
  "47|53\n\
  \97|13\n\
  \97|61\n\
  \97|47\n\
  \75|29\n\
  \61|13\n\
  \75|53\n\
  \29|13\n\
  \97|29\n\
  \53|29\n\
  \61|53\n\
  \97|53\n\
  \61|29\n\
  \47|13\n\
  \75|47\n\
  \97|75\n\
  \47|61\n\
  \75|61\n\
  \47|29\n\
  \75|13\n\
  \53|13\n\
  \\n\
  \75,47,61,53,29\n\
  \97,61,53,29,13\n\
  \75,29,13\n\
  \75,97,47,61,53\n\
  \61,13,29\n\
  \97,13,75,29,47\n"

type Rules = [(Int, Int)]

parse :: String -> (Rules, [[Int]])
parse s = (map parseRule rules, map parsePrint prints)
 where
  (rules, prints) = splitOn "" $ lines s
  parseRule r =
    let (r1, r2) = splitOn '|' r
     in (read r2, read r1)
  parsePrint p = read $ "[" ++ p ++ "]"

violates :: Rules -> [Int] -> Bool
violates rs ps = any doesViolate (pageOrderings ps)
 where
  pageOrderings [] = []
  pageOrderings (x : xs) = map (x,) xs ++ pageOrderings xs

  doesViolate :: (Int, Int) -> Bool
  doesViolate pages = pages `elem` rs

middleElement :: [a] -> a
middleElement xs = xs !! (length xs `div` 2)

-- >>> solve1 example
-- 143
solve1 :: [Char] -> Int
solve1 input = sum $ map middleElement $ filter (not . violates rules) updates
 where
  (rules, updates) = parse input

-- >>> solve2 example
-- 123
solve2 :: Solution
solve2 input = sum $ map (middleElement . reorder) $ filter (violates rules) updates
 where
  (rules, updates) = parse input

  reorder = sortBy (\x y -> if (x, y) `elem` rules then LT else GT)
