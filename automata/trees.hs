module Trees where

import DPLL (Clause (Or), Literal (N, P))
import Data.List (nub, sortOn)
import Debug.Trace

data Exp
  = Lit Int
  | Exp `Add` Exp
  | Exp `Sub` Exp
  | Exp `Mul` Exp
  | Exp `Div` Exp
  | If Cond Exp Exp
  deriving (Eq)

data Cond
  = Eq Exp Exp
  | Lt Exp Exp
  | Gt Exp Exp
  deriving (Eq)

instance Show Exp where
  show (Lit n) =
    show n
  show (Add e f) =
    "(" ++ (show e ++ " + " ++ show f) ++ ")"
  show (Sub e f) =
    "(" ++ (show e ++ " - " ++ show f) ++ ")"
  show (Mul e f) =
    "(" ++ (show e ++ " * " ++ show f) ++ ")"
  show (Div e f) =
    "(" ++ (show e ++ " / " ++ show f) ++ ")"
  show (If c e f) = "if " ++ show c ++ " then " ++ show e ++ " else " ++ show f

instance Show Cond where
  show (Eq e f) = show e ++ " == " ++ show f
  show (Lt e f) = show e ++ " < " ++ show f
  show (Gt e f) = show e ++ " > " ++ show f

evalExp :: Exp -> Int
evalExp (Lit n) = n
evalExp (Add e f) = evalExp e + evalExp f
evalExp (Sub e f) = evalExp e - evalExp f
evalExp (Mul e f) = evalExp e * evalExp f
evalExp (Div e f) = evalExp e `div` evalExp f
evalExp (If c e f) = if evalCond c then evalExp e else evalExp f

evalCond :: Cond -> Bool
evalCond (Eq e f) = evalExp e == evalExp f
evalCond (Lt e f) = evalExp e < evalExp f
evalCond (Gt e f) = evalExp e > evalExp f

e0 = Add (Lit 1) (Mul (Lit 2) (Lit 3))

e1 = Mul (Add (Lit 1) (Lit 2)) (Lit 3)

c0 = Lt e0 e1

e2 = If c0 e0 e1

data Sequent = Sequent {ante :: [Prop], post :: [Prop]} deriving (Eq, Show)

type Name = String

type Valn = Name -> Bool

type Names = [Name]

data Prop
  = Var Name
  | F
  | T
  | Not Prop
  | Prop :||: Prop
  | Prop :&&: Prop
  | Prop :->: Prop
  | Prop :<->: Prop
  deriving (Eq)

instance Show Prop where
  show (Var x) = x
  show F = "F"
  show T = "T"
  show (Not p) = "(not " ++ show p ++ ")"
  show (p :||: q) = "(" ++ show p ++ " || " ++ show q ++ ")"
  show (p :&&: q) = "(" ++ show p ++ " && " ++ show q ++ ")"
  show (p :->: q) = "(" ++ show p ++ " -> " ++ show q ++ ")"
  show (p :<->: q) = "(" ++ show p ++ " <-> " ++ show q ++ ")"

evalProp :: Valn -> Prop -> Bool
evalProp vn (Var x) = vn x
evalProp vn F = False
evalProp vn T = True
evalProp vn (Not p) = not (evalProp vn p)
evalProp vn (p :||: q) = evalProp vn p || evalProp vn q
evalProp vn (p :&&: q) = evalProp vn p && evalProp vn q
evalProp vn (p :->: q) = not (evalProp vn p) || evalProp vn q
evalProp vn (p :<->: q) = evalProp vn p == evalProp vn q

stageOne :: Prop -> Prop
stageOne (Var x) = Var x
stageOne F = F
stageOne T = T
stageOne (Not p) = Not (stageOne p)
stageOne (p :||: q) = stageOne p :||: stageOne q
stageOne (p :&&: q) = stageOne p :&&: stageOne q
stageOne (p :->: q) = stageOne (Not p) :||: stageOne q
stageOne (p :<->: q) = stageOne (p :->: q) :&&: stageOne (q :->: p)

toNNF :: Prop -> Prop
toNNF (Var x) = Var x
toNNF F = F
toNNF T = T
toNNF (p :||: q) = toNNF p :||: toNNF q
toNNF (p :&&: q) = toNNF p :&&: toNNF q
toNNF (Not (Not p)) = toNNF p
toNNF (Not (p :&&: q)) = toNNF (Not p) :||: toNNF (Not q)
toNNF (Not (p :||: q)) = toNNF (Not p) :&&: toNNF (Not q)
toNNF (Not p) = Not p

stageThree :: Prop -> Prop
stageThree (Var x) = Var x
stageThree F = F
stageThree T = T
stageThree (Not p) = Not p
stageThree (p :&&: q)
  | p == T = stageThree q
  | q == T = stageThree p
  | otherwise = stageThree p :&&: stageThree q
stageThree (p :||: (q :&&: r)) = stageThree (stageThree (stageThree p :||: stageThree q) :&&: stageThree (stageThree p :||: stageThree r))
stageThree ((p :&&: q) :||: r) = stageThree (stageThree (stageThree p :||: stageThree r) :&&: stageThree (stageThree q :||: stageThree r))
stageThree (p :||: q)
  | p == Not q = T
  | Not p == q = T
  | p == T = T
  | q == T = T
  | otherwise = stageThree p :||: stageThree q

toCNF :: Prop -> Prop
toCNF p = stageThree (toNNF (stageOne p))

-- toClause :: Prop -> Clause Name -> Clause Name
-- toClause (Var x) c = P x : c
-- toClause (Not (Var x)) c = N x : c
-- toClause F c = Or c
-- toClause T c = Or c
-- toClause (p :||: q) c = Or ((toClause p c) ++ toClause q ++ c)

size :: Prop -> Int
size (Not p) = 1 + size p
size (p :||: q) = 1 + size p + size q
size (p :&&: q) = 1 + size p + size q
size (p :->: q) = 1 + size p + size q
size (p :<->: q) = 1 + size p + size q
size p = 1

valn :: Valn
valn "a" = True
valn "b" = True
valn "c" = False
valn "d" = True

empty :: Valn
empty y = error "undefined"

extend :: Valn -> Name -> Bool -> Valn
extend vn x b y
  | x == y = b
  | otherwise = vn y

valns :: Names -> [Valn]
valns [] = [empty]
valns (x : xs) = [extend vn x b | vn <- valns xs, b <- [True, False]]

names :: Prop -> Names
names (Var x) = [x]
names F = []
names T = []
names (Not p) = names p
names (p :||: q) = nub (names p ++ names q)
names (p :&&: q) = nub (names p ++ names q)
names (p :->: q) = nub (names p ++ names q)
names (p :<->: q) = nub (names p ++ names q)

subExpressions :: Prop -> [Prop]
subExpressions (Not p) = Not p : subExpressions p
subExpressions (p :||: q) = [p :||: q] ++ subExpressions p ++ subExpressions q
subExpressions (p :&&: q) = [p :&&: q] ++ subExpressions p ++ subExpressions q
subExpressions x = [x]

satisfiable :: Prop -> Bool
satisfiable p = or [evalProp vn p | vn <- valns (names p)]

tautology :: Prop -> Bool
tautology p = and [evalProp vn p | vn <- valns (names p)]

equivalent :: Prop -> Prop -> Bool
equivalent p q = and [evalProp vn p == evalProp vn q | vn <- valns (nub (names q) ++ names p)]

ttProp :: Valn -> Prop -> [(Prop, Bool)]
ttProp vn p = [(p, evalProp vn p) | p <- sortOn size (nub (subExpressions p))]

p0 = Var "a" :&&: Not (Var "a")

p1 = (Var "a" :&&: Var "b") :||: (Not (Var "a") :&&: Not (Var "b"))

p2 = (Var "a" :&&: Not (Var "b") :&&: (Var "c" :||: (Var "d" :&&: Var "b")) :||: (Not (Var "b") :&&: Not (Var "a"))) :&&: Var "c"

p3 = Var "r" :<->: (Var "s" :->: Var "t")

p4 = Var "r" :<->: (Var "s" :<->: Var "t")

p5 = Var "r" :<->: (Var "s" :&&: Var "t")

p6 = (Var "s" :||: Var "t") :&&: (Not (Var "t") :||: Not (Var "s")) :||: Var "r"

data Mobile
  = Mobile `Rod` Mobile
  | Weight Int
  deriving (Show)

weight :: Mobile -> Int
weight (Weight w) = w
weight (l `Rod` r) = weight l + weight r

balanced :: Mobile -> Bool
balanced (Weight w) = True
balanced (l `Rod` r) = balanced l && balanced r && weight l == weight r

instance Eq Mobile where
  (Weight x) == (Weight y) = x == y
  (Weight x) == l `Rod` r = False
  l `Rod` r == (Weight x) = False
  lx `Rod` rx == ly `Rod` ry = (lx == ly && rx == ry) || (lx == ry) && (rx == ly)

a = (Weight 20 `Rod` (Weight 15 `Rod` Weight 10)) `Rod` (Weight 20 `Rod` Weight 25)

b = (Weight 20 `Rod` (Weight 10 `Rod` Weight 10)) `Rod` Weight 40

c = Weight 40 `Rod` (Weight 20 `Rod` (Weight 10 `Rod` Weight 10))