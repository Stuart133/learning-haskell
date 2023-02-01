module Chap15 where

import Control.Monad

data Season = Winter | Spring | Summer | Fall deriving (Eq, Show)

next :: Season -> Season
next Winter = Spring
next Spring = Summer
next Summer = Fall
next Fall = Winter

type Radius = Float

type Width = Float

type Height = Float

data Shape
  = Circle Radius
  | Rect Width Height
  deriving (Eq, Show)

area :: Shape -> Float
area (Circle r) = pi * r ^ 2
area (Rect w h) = w * h

data Pair a b = Pair a b deriving (Eq, Show)

data Maybe a = Nothing | Just a
  deriving (Eq, Show)

data Either a b = Left a | Right b
  deriving (Eq, Show)

data Fruit
  = Apple String Bool
  | Orange String Int
  deriving (Eq, Show)

isBloodOrange :: Fruit -> Bool
isBloodOrange (Orange name _) = name == "Tarocco" || name == "Moro" || name == "Sanguinello"
isBloodOrange _ = False

bloodOrangeSegments :: [Fruit] -> Int
bloodOrangeSegments fs = sum [n | Orange _ n <- filter isBloodOrange fs]

worms :: [Fruit] -> Int
worms fs = foldl (\i v -> if v then i + 1 else i) 0 [w | Apple _ w <- fs]

data WideList a
  = Nil
  | Cons a (WideList a)
  | Append (WideList a) (WideList a)
  deriving (Eq, Show)

lengthWide :: WideList a -> Int
lengthWide Nil = 0
lengthWide (Cons a l) = 1 + lengthWide l
lengthWide (Append l r) = lengthWide l + lengthWide r
