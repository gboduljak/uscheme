{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Evaluators.Application (eval, applyLambda, applyPrimitive) where

import Ast (LispVal (Atom, Bool, IOFunction, Lambda, List, Number, PrimitiveFunction, args, body, name, targetScopeId, varargs))
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (MonadReader (local), ask, asks)
import Control.Monad.State (evalState, get, gets, put)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import EvalMonad (EvalMonad, currentScope, deleteScope, display, enterScope, exitScope, extendScope, switchToScope)
import Evaluators.Primitives.Primitives (and, lookup, or, primitives)
import LispError (LispError (BadSpecialForm, Default, NotFunction, NumArgs))
import Scoping.Scope (Scope (parentId), id)
import Prelude hiding (and, id, lookup, or)

eval :: LispVal -> (LispVal -> EvalMonad LispVal) -> EvalMonad LispVal
eval expr@(List (func : args)) evaluate = do
  if func `elem` [Atom "and", Atom "or"]
    then do applyAndOr func args evaluate
    else do
      funcToApply <- evaluate func
      case funcToApply of
        lambda@Lambda {} -> do
          evaledArgs <- mapM evaluate args
          applyLambda lambda evaledArgs evaluate
        primitive@PrimitiveFunction {} -> do
          evaledArgs <- mapM evaluate args
          applyPrimitive primitive evaledArgs
        io@IOFunction {} -> return io
        _ -> throwError (BadSpecialForm "invalid application expression: " expr)

applyAndOr :: LispVal -> [LispVal] -> (LispVal -> EvalMonad LispVal) -> EvalMonad LispVal
applyAndOr (Atom "and") args eval = and args eval
applyAndOr (Atom "or") args eval = or args eval
applyAndOr func _ _ = throwError (BadSpecialForm "invalid application of and/or" func)

applyPrimitive :: LispVal -> [LispVal] -> EvalMonad LispVal
applyPrimitive PrimitiveFunction {name} evaledArgs = do
  case lookup name of
    (Just funcBody) -> do funcBody evaledArgs
    Nothing -> throwError (NotFunction "unrecognised primitive function: " name)

applyLambda :: LispVal -> [LispVal] -> (LispVal -> EvalMonad LispVal) -> EvalMonad LispVal
applyLambda lambda@Lambda {args, body, varargs, targetScopeId} evaledArgs evaluate = do
  if length args /= length evaledArgs && isNothing varargs
    then throwError (NumArgs (length args) evaledArgs)
    else do
      callerScope <- currentScope
      switchToScope targetScopeId

      enterScope
      lambdaScope <- currentScope
      bindArgs (zip args evaledArgs)
      bindVarArgs evaledArgs
      lambdaRetVal <- evalBodies body
      exitScope

      switchToScope (id callerScope)
      -- deleteScope (id lambdaScope) # tempting, but causes nasty infinite loop with curried functions

      return lambdaRetVal
  where
    bindArgs = traverse_ extendScope
    bindVarArgs argsValues = case varargs of
      (Just varArgBindName) -> do
        evaledVarArgs <- varArgsToBind argsValues
        do extendScope (varArgBindName, List evaledVarArgs)
      Nothing -> do currentScope
      where
        varArgsToBind allArgs = return (drop (length args) allArgs)
    evalBodies [x] = evaluate x
    evalBodies (x : xs) = evaluate x >> evalBodies xs