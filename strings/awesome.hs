module Awesome where

removeLast x =
  take (length x - 1) x

getFifth x =
  x !! 4

getLast x = 
  drop 9 x