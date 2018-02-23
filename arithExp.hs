{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module ArithExp where
import Prelude


data Var = String
          deriving (Show, Eq)

data AExp =
           Var Var
          |Lit Integer
          |UnOp UnOp AExp
          |BinOp BinOp AExp AExp
          deriving (Show, Eq)



data UnOp =
           Neg
          |Abs
          |Sign
          deriving (Show, Eq)

data BinOp =
             Add
            |Sub
            |Mult
            |Divide
            |Mod
            deriving (Show, Eq)


type Env = Var -> Integer

getValue :: Var -> Env -> Integer
getValue var env = env var

valueofZero :: Env
valueofZero _ = 0


instance Num AExp where
        e1 + e2 = BinOp Add e1 e2
        e1 - e2 = BinOp Sub e1 e2
        e1 * e2 = BinOp Mult e1 e2
        negate e = UnOp Neg e
        fromInteger n = Lit n
        abs e = UnOp Abs e
        signum e = UnOp Sign e


eval :: AExp -> Env -> Integer
eval (Var v) env = getValue v env
eval (Lit n) _ = n
eval (UnOp op x) env =
    go op (eval x env)
  where
    go :: UnOp -> Integer -> Integer
    go unop n = case unop of
                   Neg -> -n
                   Abs -> absInt n
                    where absInt :: Integer -> Integer
                          absInt nn
                                 |nn >= 0 = nn
                                 |otherwise = -nn

                   Sign -> signInt n
                    where signInt :: Integer -> Integer
                          signInt mm
                                  |mm == 0 = 0
                                  |mm < 0 = -1
                                  |mm > 0 = 1
eval (BinOp op x y) env =
   go op (eval x env) (eval y env)
 where
   go :: BinOp -> Integer -> Integer -> Integer
   go binop n m = case binop of
                            Add -> n + m
                            Sub -> n - m
                            Mult -> n * m
                            Divide -> n `div` m
                            Mod -> n `mod` m

---- Tests:
-- *ArithExp> eval (abs (-1)) valueofZero
-- 1
-- *ArithExp> eval (signum (-1)) valueofZero
-- -1
-- *ArithExp> eval (abs (2+3)) valueofZero
-- 5
-- *ArithExp> eval (abs (2-5)) valueofZero
-- 3
-- *ArithExp> eval (signum (2-5)) valueofZero
-- -1
-- *ArithExp> eval (fromInteger (2-5)) valueofZero
-- -3
