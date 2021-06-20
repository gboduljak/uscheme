module Evaluators.Set where

import Ast (LispVal (Atom, List))
import Control.Monad.Except
  ( Monad (return),
    MonadError (throwError),
  )
import EvalMonad (EvalMonad, extendScope, lookup, updateInOwningScope)
import LispError (LispError (BadSpecialForm, Default))
import Prelude hiding (lookup)

eval :: LispVal -> (LispVal -> EvalMonad LispVal) -> EvalMonad LispVal
eval expr@(List [Atom "set!", bindingName, bindingValueExpr]) evaluate = do
  case bindingName of
    (Atom name) -> do
      match <- lookup name
      case match of
        (Just _) -> do
          evaledValue <- evaluate bindingValueExpr
          do updateInOwningScope (name, evaledValue)
          return (Atom name)
        Nothing -> throwError (Default ("attempted to set! undefined reference :" ++ show bindingName))
    _ -> throwError (illFormedSet expr)
eval expr _ = throwError (illFormedSet expr)

illFormedSet :: LispVal -> LispError
illFormedSet = BadSpecialForm "ill-formed set! expression, expected binding name got:"