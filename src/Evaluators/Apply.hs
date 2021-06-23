{-# LANGUAGE NamedFieldPuns #-}

module Evaluators.Apply (eval) where

import Ast
import Control.Monad.Except (MonadTrans (lift), throwError)
import Control.Monad.Reader (ask, asks)
import Control.Monad.State (gets)
import qualified Data.Map as Map
import EvalMonad
  ( EvalMonad,
    display,
    lookup,
  )
import qualified Evaluators.Application as Application (applyLambda, applyPrimitive, eval)
import qualified Evaluators.Primitives.Primitives as Primitives (lookup, primitives)
import LispError (LispError (BadSpecialForm, UnboundVar))
import Prelude hiding (lookup)

eval :: LispVal -> (LispVal -> EvalMonad LispVal) -> EvalMonad LispVal
eval (List [Atom "apply", func, List [Atom "quote", List args]]) evaluate = do
  funcToApply <- evaluate func
  case funcToApply of
    primitive@PrimitiveFunction {name} -> Application.applyPrimitive primitive args evaluate
    lambda@Lambda {} -> Application.applyLambda lambda args evaluate
    _ -> evaluate (List (funcToApply : args))
eval expr evaluate = throwError $ BadSpecialForm "ill-formed apply expression: " expr