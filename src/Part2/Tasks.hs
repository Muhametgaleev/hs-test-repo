module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
infixl 6 |+|
infixl 6 |-|
infixl 7 |*|
(|+|) :: Term -> Term -> Term
(|+|) a b = BinaryTerm Plus a b
(|-|) :: Term -> Term -> Term
(|-|) a b = BinaryTerm Minus a b
(|*|) :: Term -> Term -> Term
(|*|) a b = BinaryTerm Times a b

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement (Variable n)
  | n == varName = replacement
  | otherwise = Variable n
replaceVar varName replacement (BinaryTerm op a b) =
  BinaryTerm op (replaceVar varName replacement a) (replaceVar varName replacement b)
replaceVar _ _ term = term

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (IntConstant n) = IntConstant n
evaluate (Variable name) = Variable name
evaluate (BinaryTerm op a b) =
  case (evaluate a, evaluate b) of
    (IntConstant x, IntConstant y) -> IntConstant (applyOp op x y)
    (ll, rr) -> BinaryTerm op ll rr
  where
    applyOp Plus = (+)
    applyOp Minus = (-)
    applyOp Times = (*)
