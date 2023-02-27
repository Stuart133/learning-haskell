module DPLL where

import Data.List (delete, intercalate)
import Trees (a)

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

(<<) :: Eq atom => [Clause atom] -> Literal atom -> [Clause atom]
cs << l = [Or (delete (neg l) ls) | Or ls <- cs, l `notElem` ls]

neg :: Literal atom -> Literal atom
neg (P a) = N a
neg (N a) = P a

dpll :: Eq atom => Form atom -> [[Literal atom]]
dpll (And []) = [[]]
dpll (And (Or [] : cs)) = []
dpll (And (Or (l : ls) : cs)) =
  [l : ls | ls <- dpll (And (cs << l))]
    ++ [neg l : ls | ls <- dpll (And (Or ls : cs << neg l))]

cnf =
  And
    [ Or [N A, N B, P C],
      Or [N A, P D, P F],
      Or [P A, P B, P E],
      Or [P A, P B, N C]
    ]