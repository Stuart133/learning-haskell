module Chap5 where

import Language.Haskell.TH (Pred)

data Thing = R | S | T | U | V | W | X | Y | Z
  deriving (Eq, Show)

type Predicate u = u -> Bool

things :: [Thing]
things = [R, S, T, U, V, W, X, Y, Z]

isDisk :: Predicate Thing
isDisk x = x `elem` [U, R, Y, Z]

isTriangle :: Predicate Thing
isTriangle x = not (isDisk x)

isSmall :: Predicate Thing
isSmall x = x `elem` [R, S, V, X]

isBig :: Predicate Thing
isBig x = not (isSmall x)

isWhite :: Predicate Thing
isWhite x = x `elem` [U, V]

isGrey :: Predicate Thing
isGrey x = x `elem` [R, S, W, Y]

isBlack :: Predicate Thing
isBlack x = not (isWhite x) && not (isGrey x)

neg :: Predicate u -> Predicate u
neg a x = not (a x)

(|=) :: Predicate Thing -> Predicate Thing -> Bool
(|=) ante succ =
  and [succ x | x <- things, ante x]

(||=) :: [Predicate Thing] -> Predicate Thing -> Bool
(||=) antes succ =
  and [succ x | x <- filter (allFilt antes) things]

(|:|) :: Predicate u -> Predicate u -> Predicate u
(|:|) a b x = a x || b x

(&:&) :: Predicate u -> Predicate u -> Predicate u
(&:&) a b x = a x && b x

allFilt :: [Predicate a] -> Predicate a
allFilt fns el = all (\fn -> fn el) fns
