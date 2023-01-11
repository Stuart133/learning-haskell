module Chap11 where

import Distribution.ModuleName (main)

prodFromTo :: Int -> Int -> Int
prodFromTo m n
  | m > n = 1
  | m <= n = m * prodFromTo (m + 1) n

factorial :: Int -> Int
factorial = prodFromTo 1

countDoubled :: String -> Int
countDoubled [] = 0
countDoubled xs = length [x | (x, y) <- zip xs (tail xs), x == y]

search :: Eq a => [a] -> a -> [Int]
search xs y = [i | (i, x) <- zip [0 ..] xs, x == y]

searchRec :: String -> Char -> [Int]
searchRec xs y = srch xs y 0
  where
    srch :: String -> Char -> Int -> [Int]
    srch [] y i = []
    srch (x : xs) y i
      | x == y = i : srch xs y (i + 1)
      | otherwise = srch xs y (i + 1)

plus :: Int -> Int -> Int
plus m 0 = 0
plus m n = plus m (n - 1) + 1

times :: Int -> Int -> Int
times m 0 = 0
times m n = plus (times m (n - 1)) m

power :: Int -> Int -> Int
power m 0 = 1
power m n = times (power m (n - 1)) m