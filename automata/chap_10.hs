{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use foldr" #-}

module Chap10 where

squares :: [Int] -> [Int]
squares [] = []
squares (x : xs) = x * x : squares xs

odds :: [Int] -> [Int]
odds [] = []
odds (x : xs)
  | odd x = x : odds xs
  | otherwise = odds xs

sumSquareOdds :: [Int] -> Int
sumSquareOdds [] = 0
sumSquareOdds (x : xs)
  | odd x = x * x + sumSquareOdds xs
  | otherwise = sumSquareOdds xs

insert :: Ord a => a -> [a] -> [a]
insert m [] = [m]
insert m (n : ns)
  | m <= n = m : n : ns
  | otherwise = n : insert m ns

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (n : ns) = insert n (insertionSort ns)

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (m : ns) = quickSort less ++ [m] ++ quickSort more
  where
    less = [n | n <- ns, n < m]
    more = [n | n <- ns, n >= m]

sortProp :: [Int] -> Bool
sortProp ns = insertionSort ns == quickSort ns