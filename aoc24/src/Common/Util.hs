module Common.Util where

type Solution = String -> Int

-- | reverse function application
(&) :: a -> (a -> b) -> b
x & f = f x

infixl 1 &

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
