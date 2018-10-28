--{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module ProbDenCalc where

import Control.Monad
import System.Random

--type Double = Double
data Var a where
  Double :: String -> Var Double
  Int :: String -> Var Int
  Bool :: String -> Var Bool

data Expr a where
  StdRandom :: Expr Double
  Lit :: Rational -> Expr Double
  Var :: Var a -> Expr a
  Let :: Var a -> Expr a -> Expr b -> Expr b
  Neg :: Expr Double -> Expr Double
  Exp :: Expr Double -> Expr Double
  Log :: Expr Double -> Expr Double
  Not :: Expr Bool -> Expr Bool
  Add :: Expr Double -> Expr Double -> Expr Double
  Less :: Expr Double -> Expr Double -> Expr Bool
  If :: Expr Bool -> Expr a -> Expr a -> Expr a

sample :: Expr a -> Env -> IO a
sample StdRandom _ = getStdRandom random
sample (Lit x) _ = return (fromRational x)
sample (Var v) env = return (lookupEnv env v)
sample (Let v e e') env = do
  x <- sample e env
  sample e' (extendEnv v x env)
sample (Neg e) env = fmap negate (sample e env)
sample (Exp e) env = fmap exp (sample e env)
sample (Log e) env = fmap log (sample e env)
sample (Not e) env = fmap not (sample e env)
sample (Add e1 e2) env = liftM2 (+) (sample e1 env) (sample e2 env)
sample (Less e1 e2) env = liftM2 (<) (sample e1 env) (sample e2 env)
sample (If e e1 e2) env = do
  b <- sample e env
  sample
    (if b
       then e1
       else e2)
    env

-- sample (Let (Double "x") StdRandom (Add (Var $ Double  "x") (Var $ Double "x"))) emptyEnv 
type Env = forall a. Var a -> a

lookupEnv :: Env -> Var a -> a
lookupEnv env = env

emptyEnv :: Env
emptyEnv v = error "Unbound"

extendEnv :: Var a -> a -> Env -> Env
extendEnv (Double v) x _ (Double v')
  | v == v' = x
extendEnv (Bool v) x _ (Bool v')
  | v == v' = x
extendEnv _ _ env v' = env v'

exponential :: Expr Double
exponential = Neg (Log StdRandom)

integral :: Double -> Double -> (Double -> Double) -> Double
integral = undefined

expect :: Expr a -> Env -> (a -> Double) -> Double
expect StdRandom _ c = integral 0 1 c
expect (Lit x) _ c = c (fromRational x)
expect (Var v) env c = c (lookupEnv env v)
expect (Let v e e') env c = expect e env (\x -> expect e' (extendEnv v x env) c)
expect (Neg e) env c = expect e env (c . negate)
expect (Exp e) env c = expect e env (c . exp)
expect (Log e) env c = expect e env (c . log)
expect (Not e) env c = expect e env (c . not)
expect (Add e1 e2) env c = expect e1 env (\x -> expect e2 env (\y -> c (x + y)))
expect (Less e1 e2) env c =
  expect e1 env (\x -> expect e2 env (\y -> c (x < y)))
expect (If e e1 e2) env c =
  expect
    e
    env
    (\b ->
       expect
         (if b
            then e1
            else e2)
         env
         c)



