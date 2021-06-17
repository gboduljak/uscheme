{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module ToyMonads where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader (MonadReader (ask), Reader, ReaderT (ReaderT, runReaderT), asks, runReader)
import Control.Monad.Trans (lift)
import qualified Data.Map as Map (Map, fromList, lookup, map)

type VariablesEnv = Map.Map String Double

data Expr
  = Expr :+: Expr
  | Expr :-: Expr
  | Expr :*: Expr
  | Expr :/: Expr
  | Num Double
  | Variable String

data EvalError = DivByZero | VarNotFound String deriving (Show)

type EvalWithEnvAndExcept a = ExceptT EvalError (Reader VariablesEnv) a

eval :: Expr -> EvalWithEnvAndExcept Double
eval (Num x) = return x
eval (l :+: r) = do
  a <- eval l
  b <- eval r
  return (a + b)
eval (Variable x) = do
  env <- ask
  case Map.lookup x env of
    Just v -> return v
    _ -> throwError (VarNotFound x)
eval (l :-: r) = do
  a <- eval l
  b <- eval r
  return (a - b)
eval (l :*: r) = do
  a <- eval l
  b <- eval r
  return (a * b)
eval (l :/: r) = do
  a <- eval l
  b <- eval r
  if b == 0 then throwError DivByZero else return (a / b)

testExp = Variable "x"

testExp2 = Num 2 :/: Num 0

testEnv = Map.fromList [("x", 3.2)]

doEval :: Expr -> VariablesEnv -> Either EvalError Double
doEval expr = runReader $ runExceptT (eval expr)