{-# LANGUAGE FlexibleContexts #-}

module Evaluators.Lambda (eval) where

import Ast (LispVal (Atom, Bool, Lambda, List))
import qualified Control.Arrow as Data.Bifunctor
import Control.Monad (foldM, sequence_)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask, asks, local)
import Control.Monad.State (put)
import Control.Monad.Trans.State (gets)
import Data.Bifunctor (second)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import EvalMonad (EvalMonad, currentScope, enterScope, exitScope, extendScope)
import Evaluators.FuncToolkit (buildLambda)
import LispError (LispError (BadSpecialForm))

eval :: LispVal -> EvalMonad LispVal
eval expr@(List [Atom "lambda", List args, body]) = buildLambda args body
eval expr = throwError (BadSpecialForm "ill-formed lambda definition: " expr)