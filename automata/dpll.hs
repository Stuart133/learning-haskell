module DPLL where

import Data.List (delete, intercalate, sortOn)
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
dpll f =
  case prioritise f of
    [] -> [[]]
    Or [] : cs -> []
    Or (l : ls) : cs -> [l : ls | ls <- dpll (And (cs << l))] ++ [neg l : ls | ls <- dpll (And (Or ls : cs << neg l))]

prioritise :: Form atom -> [Clause atom]
prioritise (And cs) = sortOn (\(Or ls) -> length ls) cs

cnf =
  And
    [ Or [N A, N B, P C],
      Or [N A, P D, P F],
      Or [P A, P B, P E],
      Or [P A, P B, N C]
    ]

allFilled :: Form (Int, Int, Int)
allFilled =
  And
    [ Or [P (i, j, n) | n <- [1 .. 9]]
      | i <- [1 .. 9],
        j <- [1 .. 9]
    ]

noneFilledTwice :: Form (Int, Int, Int)
noneFilledTwice =
  And
    [ Or [N (i, j, n), N (i, j, n')]
      | i <- [1 .. 9],
        j <- [1 .. 9],
        n <- [1 .. 9],
        n' <- [1 .. (n - 1)]
    ]

rowsComplete :: Form (Int, Int, Int)
rowsComplete =
  And
    [ Or [P (i, j, n) | j <- [1 .. 9]]
      | i <- [1 .. 9],
        n <- [1 .. 9]
    ]

rowsNoRepetition :: Form (Int, Int, Int)
rowsNoRepetition =
  And
    [ Or [N (i, j, n), N (i, j', n)]
      | i <- [1 .. 9],
        n <- [1 .. 9],
        j <- [1 .. 9],
        j' <- [1 .. (j - 1)]
    ]

columnComplete :: Form (Int, Int, Int)
columnComplete =
  And
    [ Or [P (i, j, n) | i <- [1 .. 9]]
      | j <- [1 .. 9],
        n <- [1 .. 9]
    ]

columnNoRepetitions :: Form (Int, Int, Int)
columnNoRepetitions =
  And
    [ Or [N (i, j, n), N (i', j, n)]
      | i <- [1 .. 9],
        n <- [1 .. 9],
        j <- [1 .. 9],
        i' <- [1 .. (i - 1)]
    ]

squareComplete :: Form (Int, Int, Int)
squareComplete =
  And
    [ Or
        [P (i, j, n) | n <- [1 .. 9]]
      | (i, j) <- concat macroSquares
    ]

macroSquares :: [[(Int, Int)]]
macroSquares =
  [ [ (i, j) | i <- [((x - 1) * 3) + 1 .. x * 3], j <- [((y - 1) * 3) + 1 .. y * 3]
    ]
    | x <- [1 .. 3],
      y <- [1 .. 3]
  ]