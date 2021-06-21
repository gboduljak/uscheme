module Evaluators.Atom (eval) where

import Ast
import Control.Monad.Except (MonadTrans (lift), throwError)
import Control.Monad.Reader (ask, asks)
import Control.Monad.State (gets)
import qualified Data.Map as Map
import EvalMonad
  ( EvalMonad,
    lookup,
  )
import qualified Evaluators.Primitives.Primitives as Primitives (lookup, primitives)
import LispError (LispError (BadSpecialForm, UnboundVar))
import Prelude hiding (lookup)

eval :: LispVal -> EvalMonad LispVal
eval (Atom name) = do
  result <- lookup name
  case result of
    (Just value) -> return value
    _ -> case Primitives.lookup name of
      (Just primitive) -> return (PrimitiveFunction name)
      _ -> throwError (UnboundVar "attempted to retrieve unbound variable" name)
eval expr = throwError $ BadSpecialForm "ill-formed atom expression: " expr