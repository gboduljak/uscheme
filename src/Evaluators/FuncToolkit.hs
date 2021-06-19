{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Evaluators.FuncToolkit (buildFunction, buildLambda) where

import Ast (LispVal (Atom, Lambda, args, body, lambdaId))
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (ask)
import Control.Monad.State (get, gets, put)
import Data.Functor (($>))
import qualified Data.Map as Map
import EvalMonad (EvalMonad, EvaluationEnv (Env, isGlobal, variables), EvaluationState (..), unusedLambdaId)
import LispError (LispError (BadSpecialForm, Default))

-- function is implemented as a named lambda, in the spirit of Scheme define
buildFunction :: String -> [LispVal] -> LispVal -> EvalMonad LispVal
buildFunction name args body = do
  env <- ask
  globalEnv <- gets globalEnv
  lambdaCtxes <- gets lambdaContexts
  builtLambda <- buildLambda args body
  case builtLambda of
    lambda@Lambda {} ->
      put
        St
          { globalEnv = globalEnv,
            lambdaContexts =
              Map.insert
                (lambdaId lambda)
                ( Env
                    { variables = Map.insert name lambda (variables env),
                      isGlobal = isGlobal env
                    }
                )
                lambdaCtxes
          }
        $> lambda

buildLambda :: [LispVal] -> LispVal -> EvalMonad LispVal
buildLambda lambdaArgs lambdaBody = do
  env <- ask
  globalEnv <- gets globalEnv
  lambdaCtxes <- gets lambdaContexts
  state <- get
  args <- mapM unpackAtomId lambdaArgs
  let lambdaId = unusedLambdaId state
   in put
        St
          { globalEnv = globalEnv,
            lambdaContexts = Map.insert lambdaId env (lambdaContexts state)
          }
        $> Lambda {lambdaId = lambdaId, args = args, body = lambdaBody}

unpackAtomId :: LispVal -> EvalMonad String
unpackAtomId (Atom x) = return x
unpackAtomId val = throwError (BadSpecialForm "tried to define lambda with arg: " val)