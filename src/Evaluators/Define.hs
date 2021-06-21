module Evaluators.Define (eval) where

import Ast (LispVal (Atom, Bool, DottedList, Lambda, List, varargs))
import qualified Control.Arrow as Data.Bifunctor
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask, asks, local)
import Control.Monad.State (put)
import Control.Monad.Trans.State (gets)
import Data.Bifunctor (second)
import Data.Functor (($>))
import EvalMonad (EvalMonad, extendScope)
import Evaluators.FuncToolkit (buildFunction, buildLambda)
import LispError (LispError (BadSpecialForm))

eval :: LispVal -> (LispVal -> EvalMonad LispVal) -> EvalMonad LispVal
eval expr@(List (Atom "define" : List (Atom funcName : funcArgs) : funcBody)) evaluate = do
  lambda <- buildLambda funcArgs Nothing funcBody
  extendScope (funcName, lambda)
  return (Atom funcName)
eval expr@(List (Atom "define" : DottedList (Atom funcName : funcArgs) (Atom varargs) : funcBody)) evaluate = do
  lambda <- buildLambda funcArgs (Just varargs) funcBody
  extendScope (funcName, lambda)
  return (Atom funcName)
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