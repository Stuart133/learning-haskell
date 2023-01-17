module Chap13 where

import Data.Char

sumSquareOdds :: [Int] -> Int
sumSquareOdds = foldr ((+) . (^ 2)) 0 . filter odd

compare :: [(String, String)]
compare = filter (uncurry (<)) (zip ["one", "two", "three", "four"] ["eins", "zwei", "drei", "vier"])

iter :: Int -> (a -> a) -> (a -> a)
iter n f = iter_rec (n - 1) f f
  where
    iter_rec n f f'
      | n <= 0 = f'
      | n > 0 = iter_rec (n - 1) f (f' . f)

iter' :: Int -> (a -> a) -> (a -> a)
iter' n f x = foldr ($) x (replicate n f)

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x : xs)
  | p x = x : takeWhile' p xs
  | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x : xs)
  | p x = dropWhile' p xs
  | otherwise = x : xs

words' :: String -> [String]
words' "" = []
words' s = takeWhile' (not . isSpace) s : words' (dropWhile' isSpace (dropWhile' (not . isSpace) s))

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs [] = []
zipWith' f [] ys = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

zipWithMap :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithMap f xs ys = map (uncurry f) (zip xs ys)

invertUnzip :: ([a], [b]) -> [(a, b)]
invertUnzip = uncurry zip

type Church a = (a -> a) -> a -> a

church :: Int -> Church a
church = iter

succ' :: Church a -> Church a
succ' cm f x = f (cm f x)

plus :: Church a -> Church a -> Church a
plus cm cn f x = cm f (cn f x)

unchurch :: Church Int -> Int
unchurch cn = cn (+ 1) 0