{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use map" #-}

module Chap12 where

enumPattern :: (Int -> t -> t) -> t -> Int -> Int -> t
enumPattern f e m n
  | m > n = e
  | m <= n = f m (enumPattern f e (m + 1) n)

squares :: [Int] -> [Int]
squares = map sqr
  where
    sqr x = x * x

mapRec :: (a -> b) -> [a] -> [b]
mapRec f [] = []
mapRec f (x : xs) = f x : mapRec f xs

allPosDiv3 :: [Int] -> Bool
allPosDiv3 = all (div 3)
  where
    div m x = mod x m == 0

rmChar :: Char -> String -> String
rmChar c = filter (notEqual c)
  where
    notEqual c x = c /= x

rmChars :: String -> String -> String
rmChars a b = foldr rmChar b a

halveEvensHO :: [Int] -> [Int]
halveEvensHO a = map halve (filter even a)
  where
    halve x = div x 2

countPositivesHO :: [Int] -> Int
countPositivesHO a = length (filter positive a)
  where
    positive x = x >= 0