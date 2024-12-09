module Common.Util where

import Data.Array (Array)
import Data.Array.Base (listArray)

type Solution = String -> Int

parseGrid :: String -> Array (Int, Int) Char
parseGrid s = listArray ((0, 0), dims) xs
 where
  rows = lines s
  dims = (length rows - 1, length (head rows) - 1)
  xs = concat rows

allEq :: (Eq a) => [a] -> Bool
allEq [] = True
allEq (x : xs) = all (x ==) xs

zipWith' :: (a -> a -> b) -> [[a]] -> [b]
zipWith' f [xs, ys] = zipWith f xs ys
zipWith' _ _ = error "list has to have a length of 2"

countElem :: (Eq a) => a -> [a] -> Int
countElem x ls = go ls 0
 where
  go [] acc = acc
  go (y : ys) acc
    | y == x = go ys (acc + 1)
    | otherwise = go ys acc

splitOn :: (Eq a) => a -> [a] -> ([a], [a])
splitOn e = go []
 where
  go acc [] = (acc, [])
  go acc (x : xs)
    | x == e = (reverse acc, xs)
    | otherwise = go (x : acc) xs

distinct :: (Eq a) => [a] -> [a]
distinct ls = go ls []
 where
  go [] acc = acc
  go (x : xs) acc
    | x `elem` acc = go xs acc
    | otherwise = go xs (x : acc)
