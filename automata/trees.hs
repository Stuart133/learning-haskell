module Trees where

import Data.List (nub, sortOn)

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
  deriving (Eq)

instance Show Prop where
  show (Var x) = x
  show F = "F"
  show T = "T"
  show (Not p) = "(not " ++ show p ++ ")"
  show (p :||: q) = "(" ++ show p ++ " || " ++ show q ++ ")"
  show (p :&&: q) = "(" ++ show p ++ " && " ++ show q ++ ")"

evalProp :: Valn -> Prop -> Bool
evalProp vn (Var x) = vn x
evalProp vn F = False
evalProp vn T = True
evalProp vn (Not p) = not (evalProp vn p)
evalProp vn (p :||: q) = evalProp vn p || evalProp vn q
evalProp vn (p :&&: q) = evalProp vn p && evalProp vn q

size :: Prop -> Int
size (Not p) = 1 + size p
size (p :||: q) = 1 + size p + size q
size (p :&&: q) = 1 + size p + size q
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

satisfiable :: Prop -> Bool
satisfiable p = or [evalProp vn p | vn <- valns (names p)]

subExpressions :: Prop -> [Prop]
subExpressions (Not p) = Not p : subExpressions p
subExpressions (p :||: q) = [p :||: q] ++ subExpressions p ++ subExpressions q
subExpressions (p :&&: q) = [p :&&: q] ++ subExpressions p ++ subExpressions q
subExpressions x = [x]

ttProp :: Valn -> Prop -> [(Prop, Bool)]
ttProp vn p = [(p, evalProp vn p) | p <- sortOn size (nub (subExpressions p))]

p0 = Var "a" :&&: Not (Var "a")

p1 = (Var "a" :&&: Var "b") :||: (Not (Var "a") :&&: Not (Var "b"))

p2 = (Var "a" :&&: Not (Var "b") :&&: (Var "c" :||: (Var "d" :&&: Var "b")) :||: (Not (Var "b") :&&: Not (Var "a"))) :&&: Var "c"