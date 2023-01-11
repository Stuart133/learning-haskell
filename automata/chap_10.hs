{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use foldr" #-}

module Chap10 where

import Data.Char

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

merge :: Ord a => [a] -> [a] -> [a]
merge l [] = l
merge [] r = r
merge (l : ls) (r : rs)
  | l < r = l : merge ls (r : rs)
  | otherwise = r : merge (l : ls) rs

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [a] = [a]
mergeSort ns = merge (mergeSort left) (mergeSort right)
  where
    left = take (div (length ns) 2) ns
    right = drop (div (length ns) 2) ns

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (m : ns) = quickSort less ++ [m] ++ quickSort more
  where
    less = [n | n <- ns, n < m]
    more = [n | n <- ns, n >= m]

quickSortAscendingProp :: [Int] -> Bool
quickSortAscendingProp ns = isSorted (quickSort ns)

quickSortSameElementsProp :: [Int] -> Bool
quickSortSameElementsProp ns = sameElements (quickSort ns) ns

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [n] = True
isSorted (n : ns)
  | n <= head ns = isSorted ns
  | otherwise = False

sameElements :: [Int] -> [Int] -> Bool
sameElements [] b = True
sameElements (a : as) b
  | a `elem` b = sameElements as b
  | otherwise = False

sortProp :: [Int] -> Bool
sortProp ns = insertionSort ns == quickSort ns && quickSort ns == mergeSort ns

halveEvens :: [Int] -> [Int]
halveEvens [] = []
halveEvens (x : xs)
  | even x = div x 2 : halveEvens xs
  | otherwise = halveEvens xs

inRange :: Int -> Int -> [Int] -> [Int]
inRange a b [] = []
inRange a b (x : xs)
  | x >= a && x <= b = x : inRange a b xs
  | otherwise = inRange a b xs

countPositives :: [Int] -> Int
countPositives [] = 0
countPositives (x : xs)
  | x >= 0 = 1 + countPositives xs
  | otherwise = countPositives xs

multDigits :: String -> Int
multDigits "" = 1
multDigits (c : cs)
  | isDigit c = digitToInt c * multDigits cs
  | otherwise = multDigits cs
