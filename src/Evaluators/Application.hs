{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Evaluators.Application (eval) where

import Ast (LispVal (Atom, Bool, Lambda, List, Number, PrimitiveFunction, args, body, name, targetScopeId))
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (MonadReader (local), ask, asks)
import Control.Monad.State (evalState, get, gets, put)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import qualified Data.Map as Map
import EvalMonad (EvalMonad, currentScope, deleteScope, enterScope, exitScope, extendScope, switchToScope)
import Evaluators.Primitives (primitives)
import qualified Evaluators.Primitives as Primitives (lookup)
import LispError (LispError (BadSpecialForm, Default, NotFunction, NumArgs))
import Scoping.Scope (Scope (parentId), id)
import Prelude hiding (id)

eval :: LispVal -> (LispVal -> EvalMonad LispVal) -> EvalMonad LispVal
eval expr@(List (head : args)) evaluate = do
  funcToApply <- evaluate head
  case funcToApply of
    lambda@Lambda {} -> applyLambda lambda args evaluate
    primitive@PrimitiveFunction {} -> applyPrimitive primitive args evaluate
    _ -> throwError (BadSpecialForm "Invalid application expression: " expr)

applyPrimitive :: LispVal -> [LispVal] -> (LispVal -> EvalMonad LispVal) -> EvalMonad LispVal
applyPrimitive PrimitiveFunction {name} args evaluate = do
  case Primitives.lookup name of
    (Just funcBody) -> do
      evaledArgs <- mapM evaluate args
      funcBody evaledArgs
    Nothing -> throwError (NotFunction "Unrecognised primitive function: " name)

applyLambda :: LispVal -> [LispVal] -> (LispVal -> EvalMonad LispVal) -> EvalMonad LispVal
applyLambda lambda@Lambda {args, body, targetScopeId} argExprs evaluate = do
  if length args == length argExprs
    then do
      argsValues <- mapM evaluate argExprs
      callerScope <- currentScope
      switchToScope targetScopeId

      enterScope
      lambdaScope <- currentScope
      bind (zip args argsValues)
      lambdaRetVal <- evaluate body
      exitScope

      switchToScope (id callerScope)
      -- deleteScope (id lambdaScope) # tempting, but causes nasty infinite loop with curried functions

      return lambdaRetVal
    else throwError (NumArgs (length args) argExprs)
  where
    bind = traverse_ extendScope