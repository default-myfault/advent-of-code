module Day04 (solve) where

import Common.Util (Solution)
import Data.Array
import Data.List (transpose)

solve :: (Solution, Solution)
solve = (solve1, solve2)

example :: String
example =
  "\
  \MMMSXXMASM\n\
  \MSAMXMSMSA\n\
  \AMXSXMAAMM\n\
  \MSAMASMSMX\n\
  \XMASAMXAMM\n\
  \XXAMMXXAMA\n\
  \SMSMSASXSS\n\
  \SAXAMASAAA\n\
  \MAMMMXMMMM\n\
  \MXMXAXMASX"

findXmas :: String -> Int
findXmas = go 0
 where
  go acc [] = acc
  go acc xs
    | take 4 xs `elem` ["XMAS", "SAMX"] = go (acc + 1) (tail xs)
    | otherwise = go acc (tail xs)

{- | This a a terrible crime but it works.
This functions creates a list of all rows, columns and diagonal lines
-}
paths :: [[a]] -> [[[a]]]
paths s = map (\f -> f s) forwards
 where
  forwards :: [[[a]] -> [[a]]]
  forwards = [id, transpose, diagonal1, diagonal1', diagonal2, diagonal2']

  diagonal1 = transpose . zipWith drop [0 ..]
  diagonal1' = drop 1 . diagonal1 . transpose

  diagonal2 = diagonal1 . map reverse
  diagonal2' = drop 1 . diagonal2 . transpose . map reverse . reverse

-- >>> solve1 example
-- 18
solve1 :: Solution
solve1 = sum . map (sum . map findXmas) . paths . lines

-- >>> solve2 example
-- 9
solve2 :: Solution
solve2 input = length $ filter isXmas as
 where
  parsed = lines input
  dims = (length $ head parsed, length parsed)
  puzzle = array ((0, 0), dims) [((x, y), (parsed !! y) !! x) | x <- [0 .. fst dims - 1], y <- [0 .. snd dims - 1]]

  as = filter (\(i, c) -> notBorder i && c == 'A') (assocs puzzle)
  notBorder (x, y) = x /= 0 && y /= 0 && x < fst dims - 1 && y < snd dims - 1

  isXmas ((x, y), _) = nw /= se && ne /= sw && all (`elem` "MS") [nw, ne, sw, se]
   where
    nw = puzzle ! (x - 1, y + 1)
    ne = puzzle ! (x + 1, y + 1)
    sw = puzzle ! (x - 1, y - 1)
    se = puzzle ! (x + 1, y - 1)
