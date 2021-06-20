{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Evaluators.Application (eval) where

import Ast (LispVal (Atom, Bool, Lambda, List, Number, PrimitiveFunction, args, body, name, targetScopeId, varargs))
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (MonadReader (local), ask, asks)
import Control.Monad.State (evalState, get, gets, put)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import qualified Data.Map as Map
import Data.Maybe (isNothing)
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
applyLambda lambda@Lambda {args, body, varargs, targetScopeId} argExprs evaluate = do
  if length args /= length argExprs && isNothing varargs
    then throwError (NumArgs (length args) argExprs)
    else do
      argsValues <- mapM evaluate argExprs
      callerScope <- currentScope
      switchToScope targetScopeId

      enterScope
      lambdaScope <- currentScope
      bindArgs (zip args argsValues)
      bindVarArgs
      lambdaRetVal <- last <$> mapM evaluate body
      exitScope

      switchToScope (id callerScope)
      -- deleteScope (id lambdaScope) # tempting, but causes nasty infinite loop with curried functions

      return lambdaRetVal
  where
    bindArgs = traverse_ extendScope
    bindVarArgs = case varargs of
      (Just varArgBindName) -> do extendScope (varArgBindName, List varArgsToBind)
      Nothing -> do currentScope
      where
        varArgsToBind = drop (length args) argExprs
