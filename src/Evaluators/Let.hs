{-# LANGUAGE FlexibleContexts #-}

module Evaluators.Let (eval) where

import Ast (LispVal (Atom, Bool, Lambda, List))
import qualified Control.Arrow as Data.Bifunctor
-- import qualified Data.Map as Map
-- import EvalMonad
--   ( EvalMonad,
--     EvaluationEnv (..),
--     EvaluationState (St, globalEnv, lambdaContexts),
--   )

-- import qualified Data.Map as Map
-- import EvalMonad
--   ( EvalMonad,
--     EvaluationEnv (..),
--     EvaluationState (St, globalEnv, lambdaContexts),
--   )
-- import qualified Data.Map as Map
-- import EvalMonad
--   ( EvalMonad,
--     EvaluationEnv (..),
--     EvaluationState (St, globalEnv, lambdaContexts),
--   )

-- import qualified Data.Map as Map
-- import EvalMonad
--   ( EvalMonad,
--     EvaluationEnv (..),
--     EvaluationState (St, globalEnv, lambdaContexts),
--   )

import Control.Monad (foldM, sequence_)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask, asks, local)
import Control.Monad.State (put)
import Control.Monad.Trans.State (gets)
import Data.Bifunctor (second)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import EvalMonad (EvalMonad, currentScope, enterScope, exitScope, extendScope)
import LispError (LispError (BadSpecialForm))

eval :: LispVal -> (LispVal -> EvalMonad LispVal) -> EvalMonad LispVal
eval (List [Atom "let", List pairs, body]) evaluate = do
  varNames <- mapM extractPairVarName pairs
  varValues <- mapM evalPairVarBinding pairs
  let bindingsToInstall = zip varNames varValues
  enterScope
  traverse_ extendScope bindingsToInstall
  evaledBody <- evaluate body
  exitScope
  return evaledBody
  where
    extractPairVarName expr@(List [varName, varBindExpr]) =
      case varName of
        (Atom name) -> return name
        _ -> throwError $ BadSpecialForm "ill-formed let pair expression: " expr
    extractPairVarName expr = throwError $ BadSpecialForm "ill-formed let binding name expression: " expr

    evalPairVarBinding (List [varName, varBindExpr]) = evaluate varBindExpr
    evalPairVarBinding expr = throwError $ BadSpecialForm "ill-formed let binding value expression: " expr
eval expr _ = throwError $ BadSpecialForm "ill-formed let pair expression: " expr
