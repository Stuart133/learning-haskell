module DPLL where

import Data.List (delete, intercalate, sortOn)
import Trees (a)

data Var = A | B | C | D | E | F | G | H
  deriving (Eq, Show)

data Literal atom = P atom | N atom
  deriving (Eq)

instance Show a => Show (Literal a) where
  show (P x) = show x
  show (N x) = ""

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

columnsComplete :: Form (Int, Int, Int)
columnsComplete =
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

squaresComplete :: Form (Int, Int, Int)
squaresComplete =
  And (map (\xs -> Or [P x | x <- xs]) squares)

squares :: [[(Int, Int, Int)]]
squares =
  [ [ (i, j, n) | i <- [((x - 1) * 3) + 1 .. x * 3], j <- [((y - 1) * 3) + 1 .. y * 3]
    ]
    | x <- [1 .. 3],
      y <- [1 .. 3],
      n <- [1 .. 9]
  ]

(<&&>) :: Form a -> Form a -> Form a
And xs <&&> And ys = And (xs ++ ys)

sudokuProblem :: Form (Int, Int, Int)
sudokuProblem =
  And
    [ Or [P (1, 2, 9)],
      Or [P (1, 9, 2)],
      Or [P (2, 5, 9)],
      Or [P (2, 7, 4)],
      Or [P (2, 8, 6)],
      Or [P (2, 9, 3)],
      Or [P (3, 1, 3)],
      Or [P (3, 3, 6)],
      Or [P (3, 6, 8)],
      Or [P (3, 7, 1)],
      Or [P (4, 1, 6)],
      Or [P (4, 4, 9)],
      Or [P (4, 7, 3)],
      Or [P (5, 1, 9)],
      Or [P (5, 4, 8)],
      Or [P (5, 6, 2)],
      Or [P (5, 9, 1)],
      Or [P (6, 3, 2)],
      Or [P (6, 6, 7)],
      Or [P (6, 9, 5)],
      Or [P (7, 3, 3)],
      Or [P (7, 4, 5)],
      Or [P (7, 7, 7)],
      Or [P (7, 9, 4)],
      Or [P (8, 1, 5)],
      Or [P (8, 2, 1)],
      Or [P (8, 3, 7)],
      Or [P (8, 5, 8)],
      Or [P (9, 1, 4)],
      Or [P (9, 8, 1)]
    ]

soduku = allFilled <&&> noneFilledTwice <&&> rowsComplete <&&> columnsComplete <&&> squaresComplete <&&> columnNoRepetitions <&&> rowsNoRepetition <&&> sudokuProblem