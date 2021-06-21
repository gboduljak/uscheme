{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Evaluators.FuncToolkit where

import Ast (LispVal (..))
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (ask)
import Control.Monad.State (get, gets, put)
import Data.Functor (($>))
import qualified Data.Map as Map
import EvalMonad (EvalMonad, currentScope, enterScope, exitScope, extendScope)
import LispError (LispError (BadSpecialForm, Default))
import Scoping.Scope (Scope (id))
import Prelude hiding (id)

buildFunction :: String -> [LispVal] -> [LispVal] -> EvalMonad LispVal
buildFunction funcName args = buildLambda args Nothing

buildLambda :: [LispVal] -> Maybe String -> [LispVal] -> EvalMonad LispVal
buildLambda lambdaArgs varArg lambdaBody = do
  lambdaArgNames <- mapM getAtomValue lambdaArgs
  lambdaScopeId <- currentScope
  let lambda =
        Lambda
          { args = lambdaArgNames,
            body = lambdaBody,
            targetScopeId = id lambdaScopeId,
            varargs = varArg
          }
  return lambda

getAtomValue :: LispVal -> EvalMonad String
getAtomValue (Atom x) = return x
getAtomValue val = throwError (BadSpecialForm "tried to define lambda with arg: " val)