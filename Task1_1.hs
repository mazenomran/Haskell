module Task1_1 where

import Todo(todo)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ lhv :: Term, rhv :: Term } -- бинарная операция
            deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) l r = BinaryTerm Plus l r
infixl 6 |+|
(|-|) :: Term -> Term -> Term
(|-|) l r = BinaryTerm Minus l r
infixl 6 |-|
(|*|) :: Term -> Term -> Term
(|*|) l r =  BinaryTerm Times l r
infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = 
 let replace hv = replaceVar varName replacement hv in
        case expression of
            Variable variable | variable == varName -> replacement
            BinaryTerm operartion lhv rhv -> BinaryTerm operartion (replace lhv) (replace rhv)
            _ -> expression 
-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expression = case expression of
    BinaryTerm operartion lhs rhs ->
        case (operartion, left, right) of 
            (Plus, IntConstant left, IntConstant right) -> IntConstant (left + right)
            (Minus, IntConstant left, IntConstant right) -> IntConstant (left - right)
            (Times, IntConstant left, IntConstant right) -> IntConstant (left * right)
  
            _ -> BinaryTerm operartion left right 
        where
            left  = evaluate lhs
            right = evaluate rhs
    _ -> expression