module Tuples where

fst' :: (a, b) -> a
fst' (a, b) = a

snd' :: (a, b) -> b
snd' (a, b) = b

topFunc :: (Int, [a])
        -> (Int, [a])
        -> (Int, [a])
topFunc (a, b) (c, d) =
  ((a + c), (b ++ d))

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y =
  ((snd x, snd y), (fst x, fst y))