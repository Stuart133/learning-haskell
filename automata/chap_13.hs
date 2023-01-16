module Chap13 where

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
