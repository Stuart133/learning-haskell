module Chap15 where

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

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

data Maybe a = Nothing | Just a
  deriving (Eq, Show)

data Either a b = Left a | Right b
  deriving (Eq, Show)