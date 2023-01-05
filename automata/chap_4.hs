module Chap4 where

angleVectors :: (Float, Float) -> (Float, Float) -> Float
angleVectors a b = acos phi
  where
    phi = dotProduct a b / (lengthVector a * lengthVector b)

dotProduct :: (Float, Float) -> (Float, Float) -> Float
dotProduct a b =
  (fst a * fst b) + (snd a * snd b)

lengthVector :: (Float, Float) -> Float
lengthVector vec =
  sqrt (dotProduct vec vec)

type Line = (Float, Float)

intersect :: Line -> Line -> (Float, Float)
intersect a b = (x, (fst a * x) + snd a)
  where
    x = (snd b - snd a) / (fst a - fst b)

halveEvens :: [Int] -> [Int]
halveEvens xs =
  [div x 2 | x <- xs, even x]

inRange :: Int -> Int -> [Int] -> [Int]
inRange lower upper xs =
  [x | x <- xs, x >= lower, x <= upper]

countPositives :: [Int] -> Int
countPositives xs =
  length [x | x <- xs, x >= 0]
