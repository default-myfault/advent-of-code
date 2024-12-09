{-# HLINT ignore "Move brackets to avoid $" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use uncurry" #-}

module Day06 (solve) where

import Common.Util (Solution, distinct, parseGrid)
import Data.Array (Array, assocs, bounds)
import Data.Foldable (find)
import Data.Maybe (fromJust)
import GHC.Arr ((!), (//))

type Map = Array (Int, Int) Char

solve :: (Solution, Solution)
solve = (solve1, solve2)

example :: String
example =
  "\
  \....#.....\n\
  \.........#\n\
  \..........\n\
  \..#.......\n\
  \.......#..\n\
  \..........\n\
  \.#..^.....\n\
  \........#.\n\
  \#.........\n\
  \......#..."

parse :: String -> Map
parse = parseGrid

findGuard :: Map -> ((Int, Int), Char)
findGuard = fromJust . find (isGuard . snd) . assocs
 where
  isGuard c = c `elem` "^v><"

isWall :: Char -> Bool
isWall '#' = True
isWall _ = False

(>+) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(y1, x1) >+ (y2, x2) = (y1 + y2, x1 + x2)

path :: (Int, Int) -> Map -> [((Int, Int), (Int, Int))]
path start m = go (-1, 0) start
 where
  go dir pos
    | outOfBounds pos = []
    | outOfBounds nextPos = [(dir, pos)]
    | isWall (m ! nextPos) = (nextDir, pos) : go nextDir (pos >+ nextDir)
    | otherwise = (dir, pos) : go dir nextPos
   where
    nextPos = pos >+ dir
    nextDir = turn dir

  turn (y, x) = (x, -y)

  (h, w) = snd $ bounds m
  outOfBounds (y, x) = y < 0 || x < 0 || h < y || w < x

-- >>> solve1 example
-- 41
solve1 :: String -> Int
solve1 input = length $ distinct $ path start m
 where
  m = parse input :: Map
  (start, _) = findGuard m

cyclic :: (Eq a) => [a] -> Bool
cyclic l = go l l
 where
  go (x : xs) (_ : y : ys)
    | x == y = True
    | otherwise = go xs ys
  go _ _ = False

-- >>> solve2 example
-- 6
solve2 :: [Char] -> Int
solve2 input = length $ distinct $ map fst $ filter (cyclic . path start . snd) $ [(c, m // [(c, '#')]) | c <- candiates]
 where
  m = parse input :: Map
  (start, _) = findGuard m
  candiates = tail $ map snd $ path start m
