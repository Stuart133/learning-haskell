module DPLL where

import Data.List (intercalate)

data Var = A | B | C | D | E | F | G | H
  deriving (Eq, Show)

data Literal atom = P atom | N atom
  deriving (Eq)

instance Show a => Show (Literal a) where
  show (P x) = show x
  show (N x) = "not " ++ show x

newtype Clause atom = Or [Literal atom]
  deriving (Eq)

instance Show a => Show (Clause a) where
  show (Or ls) = "(" ++ intercalate " || " (map show ls) ++ ")"

newtype Form atom = And [Clause atom]
  deriving (Eq)

instance Show a => Show (Form a) where
  show (And ls) = intercalate " && " (map show ls)

cnf =
  And
    [ Or [N A, N B, P C],
      Or [N A, P D, P F],
      Or [P A, P B, P E],
      Or [P A, P B, N C]
    ]