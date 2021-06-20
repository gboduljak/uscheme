module Evaluators.Define (eval) where

import Ast (LispVal (Atom, Bool, DottedList, Lambda, List))
import qualified Control.Arrow as Data.Bifunctor
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask, asks, local)
import Control.Monad.State (put)
import Control.Monad.Trans.State (gets)
import Data.Bifunctor (second)
import Data.Functor (($>))
-- import qualified Data.Map as Map
-- import EvalMonad
--   ( EvalMonad,
--     EvaluationEnv (..),
--     EvaluationState (St, globalEnv, lambdaContexts),
--   )

import EvalMonad (EvalMonad, extendScope)
import Evaluators.FuncToolkit (buildFunction)
import LispError (LispError (BadSpecialForm))

eval :: LispVal -> (LispVal -> EvalMonad LispVal) -> EvalMonad LispVal
eval (List [Atom "define", DottedList args arg, funcBody]) _ = do
  let funcNameExpr = head args
  case funcNameExpr of
    (Atom funcName) -> do
      builtFunc <- buildFunction funcName (tail args ++ [arg]) funcBody
      extendScope (funcName, builtFunc)
      return (Atom funcName)
    _ -> throwError (illFormedDefineError funcNameExpr)
eval (List [Atom "define", List (funcNameExpr : funcArgs), funcBody]) _ = do
  case funcNameExpr of
    (Atom funcName) -> do
      builtFunc <- buildFunction funcName funcArgs funcBody
      extendScope (funcName, builtFunc)
      return (Atom funcName)
    _ -> throwError (illFormedDefineError funcNameExpr)
eval expr@(List [Atom "define", funcNameExpr, bindingValExpr]) evaluate = do
  case funcNameExpr of
    (Atom name) -> do
      evaledValue <- evaluate bindingValExpr
      extendScope (name, evaledValue)
      return (Atom name)
    _ -> throwError (illFormedDefineError funcNameExpr)
eval expr _ = throwError $ BadSpecialForm "ill-formed define expression: " expr

illFormedDefineError :: LispVal -> LispError
illFormedDefineError = BadSpecialForm "ill-formed define expression, expected binding name got:"