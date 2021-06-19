{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Evaluators.Application (apply) where

import Ast (LispVal (Atom, Lambda, List, PrimitiveFunction, args, body, lambdaId))
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (MonadReader (local), ask, asks)
import Control.Monad.State (get, gets, put)
import Data.Functor (($>))
import qualified Data.Map as Map
import EvalMonad (EvalMonad, EvaluationEnv (Env, isGlobal, variables), EvaluationState (..), unusedLambdaId)
import Evaluators.Primitives (primitives)
import LispError (LispError (BadSpecialForm, Default, NotFunction))

apply :: LispVal -> (LispVal -> EvalMonad LispVal) -> EvalMonad LispVal
apply expr@(List (head : args)) eval = do
  funcToApply <- eval head
  case funcToApply of
    lambda@Lambda {} -> applyLambda lambda args eval
    (PrimitiveFunction function) -> applyPrimitive function args eval
    _ -> throwError (BadSpecialForm "Unrecognized special form in application: " expr)

applyPrimitive :: String -> [LispVal] -> (LispVal -> EvalMonad LispVal) -> EvalMonad LispVal
applyPrimitive func args eval = do
  case Map.lookup func primitives of
    (Just funcImpl) -> do
      evaledArgs <- mapM eval args
      funcImpl evaledArgs
    Nothing -> throwError (NotFunction "Unrecognised primitive function: " func)

applyLambda :: LispVal -> [LispVal] -> (LispVal -> EvalMonad LispVal) -> EvalMonad LispVal
applyLambda lambda@(Lambda id args body) argsToBind eval = do
  env <- ask
  lambdaCtxts <- gets lambdaContexts
  evaledArgs <- mapM eval argsToBind
  case Map.lookup id lambdaCtxts of
    (Just lambdaEnv) -> do
      let lambdaEvalEnv = Env {variables = Map.fromList (zip args evaledArgs) <> variables lambdaEnv, isGlobal = False}
       in local (const lambdaEvalEnv) (eval body)
    _ -> throwError (BadSpecialForm "Tried to evaluate unrecognised lambda: " lambda)
applyLambda xs _ _ = throwError (BadSpecialForm "Unrecognized special form " xs)