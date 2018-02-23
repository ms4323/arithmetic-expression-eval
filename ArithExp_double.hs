{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module ArithExp where
import Prelude

data Var = String
          deriving (Show, Eq)

data AExp =
           Var Var
          |Lit Double
          |UnOp UnOp AExp
          |BinOp BinOp AExp AExp
          deriving (Show, Eq)


instance Num AExp where
      e1 + e2 = BinOp Add e1 e2
      e1 - e2 = BinOp Sub e1 e2
      e1 * e2 = BinOp Mult e1 e2
      fromInteger n = Lit (fromInteger n)
      abs e = UnOp Abs e
      signum e = UnOp Sign e
      negate e = UnOp Neg e

instance Fractional AExp where
      e1 / e2 = BinOp Divide e1 e2
      fromRational r = Lit (fromRational r)

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
            deriving (Show, Eq)


type Env = Var -> Double

getValue :: Var -> Env -> Double
getValue var env = env var

valueofZero :: Env
valueofZero _ = 0.0

eval :: AExp -> Env -> Double
eval (Var v) env = getValue v env
eval (Lit n) _ = n
eval (UnOp op x) env =
    go op (eval x env)
    where
      go :: UnOp -> Double -> Double
      go unop n = case unop of
                     Neg -> -n
                     Abs -> absInt n
                      where absInt :: Double -> Double
                            absInt nn
                                   |nn >= 0 = nn
                                   |otherwise = -nn

                     Sign -> signInt n
                      where signInt :: Double -> Double
                            signInt mm
                                    |mm == 0 = 0
                                    |mm < 0 = -1
                                    |mm > 0 = 1

eval (BinOp op x y) env =
   go op (eval x env) (eval y env)
 where
   go :: BinOp -> Double -> Double -> Double
   go binop n m = case binop of
                            Add -> n + m
                            Sub -> n - m
                            Mult -> n * m
                            Divide -> n / m

--- Tests:
-- *ArithExp> eval (3+4) valueofZero
-- 7.0
-- *ArithExp> eval (signum(3+4)) valueofZero
-- 1.0
-- *ArithExp> eval (abs(1*7)) valueofZero
-- 7.0
-- *ArithExp> eval (8/4) valueofZero
-- 2.0
