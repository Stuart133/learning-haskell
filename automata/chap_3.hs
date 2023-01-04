module Chap3 where 

root :: Float -> Float -> Float -> Float
root a b c = 
    ((-b) + sqrt (b ^ 2 - (4 * a * c))) / (2 * a)

hour :: Int -> Int
hour n = 
    (mod (div n 60) 12) + 1

between :: Int -> Int -> Int -> Int
between a b c 
    | (a >= b && a <= c) || (a <= b && a >= c) = a
    | (b >= a && b <= c) || (b <= a && b >= c) = b
    | otherwise = c

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False