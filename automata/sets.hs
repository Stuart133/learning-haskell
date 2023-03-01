module Sets where

import Data.List (nub, sort)

type SetList = [Int]

invariant :: SetList -> Bool
invariant ns = and [m < n | (m, n) <- zip ns (tail ns)]

empty :: SetList
empty = []

singleton :: Int -> SetList
singleton n = [n]

set :: [Int] -> SetList
set ns = nub (sort ns)

union :: SetList -> SetList -> SetList
ms `union` [] = ms
[] `union` ns = ns
(m : ms) `union` (n : ns)
  | n == m = m : (ms `union` ns)
  | m < n = m : (ms `union` (n : ns))
  | otherwise = n : ((m : ms) `union` ns)

element :: Int -> SetList -> Bool
m `element` [] = False
m `element` (n : ns)
  | m < n = False
  | m == n = True
  | m > n = m `element` ns

equal :: SetList -> SetList -> Bool
ms `equal` ns = ms == ns

data SetTree = Nil | Node SetTree Int SetTree
  deriving (Show)