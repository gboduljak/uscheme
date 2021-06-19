module Evaluators.Atom (resolve) where

import Ast
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask, asks)
import Control.Monad.State (gets)
import qualified Data.Map as Map
import EvalMonad
  ( EvalMonad,
    EvaluationEnv (variables),
    EvaluationState (globalEnv),
  )
import Evaluators.Primitives (primitives)
import LispError (LispError (UnboundVar))

resolve :: String -> EvalMonad LispVal
resolve name = do
  env <- ask
  globalEnv <- gets globalEnv
  case lookup env name of
    (Just value) -> return value
    _ -> case lookup globalEnv name of
      (Just value) -> return value
      _ -> throwError (UnboundVar "attempted to retrieve unbound variable" name)
  where
    lookup :: EvaluationEnv -> String -> Maybe LispVal
    lookup env target = do
      case Map.lookup target (variables env) of
        Just value -> Just value
        _ -> case Map.lookup target primitives of
          (Just value) -> Just (PrimitiveFunction target)
          _ -> Nothing