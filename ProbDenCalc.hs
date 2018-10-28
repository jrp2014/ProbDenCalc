{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module ProbDenCalc where

import Control.Monad
import System.Random

type MyReal = Double

data Var a where
  MyReal :: String -> Var MyReal
  Bool :: String -> Var Bool

data Expr a where
  StdRandom :: Expr MyReal
  Lit :: Rational -> Expr MyReal
  Var :: Var a -> Expr a
  Let :: Var a -> Expr a -> Expr b -> Expr b
  Neg :: Expr MyReal -> Expr MyReal
  Exp :: Expr MyReal -> Expr MyReal
  Log :: Expr MyReal -> Expr MyReal
  Not :: Expr Bool -> Expr Bool
  Add :: Expr MyReal -> Expr MyReal -> Expr MyReal
  Less :: Expr MyReal -> Expr MyReal -> Expr Bool
  If :: Expr Bool -> Expr a -> Expr a -> Expr a

sample :: Expr a -> Env -> IO a
sample StdRandom _ = getStdRandom random
sample (Lit x) _ = return (fromRational x)
sample (Var v) ρ = return (lookupEnv ρ v)
sample (Let v e e') ρ = do
  x <- sample e ρ
  sample e' (extendEnv v x ρ)
sample (Neg e) ρ = fmap negate (sample e ρ)
sample (Exp e) ρ = fmap exp (sample e ρ)
sample (Log e) ρ = fmap log (sample e ρ)
sample (Not e) ρ = fmap not (sample e ρ)
sample (Add e1 e2) ρ = liftM2 (+) (sample e1 ρ) (sample e2 ρ)
sample (Less e1 e2) ρ = liftM2 (<) (sample e1 ρ) (sample e2 ρ)
sample (If e e1 e2) ρ = do
  b <- sample e ρ
  sample
    (if b
       then e1
       else e2)
    ρ

-- sample (Let (MyReal "x") StdRandom (Add (Var $ MyReal  "x") (Var $ MyReal "x"))) emptyEnv 
type Env = forall a. Var a -> a

lookupEnv :: Env -> Var a -> a
lookupEnv ρ = ρ

emptyEnv :: Env
emptyEnv v = error "Unbound"

extendEnv :: Var a -> a -> Env -> Env
extendEnv (MyReal v) x _ (MyReal v')
  | v == v' = x
extendEnv (Bool v) x _ (Bool v')
  | v == v' = x
extendEnv _ _ ρ v' = ρ v'

exponential :: Expr MyReal
exponential = Neg (Log StdRandom)
