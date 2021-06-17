{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Evaluators.StringPrimitives (stringPrimitives) where

import Ast (LispVal (Atom, Bool))
import Control.Monad.Except (MonadError (throwError))
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import EvalMonad (EvalMonad)
import Evaluators.ExpToolkit (applyPredicate, applyUnaryOp, unpackStr)
import LispError (LispError (Default, NumArgs, TypeMismatch))

stringPrimitives :: [(String, [LispVal] -> EvalMonad LispVal)]
stringPrimitives =
  [ ("string=?", applyPredicate unpackStr (==)),
    ("string<?", applyPredicate unpackStr (<)),
    ("string>?", applyPredicate unpackStr (>)),
    ("string<=?", applyPredicate unpackStr (<=)),
    ("string>=?", applyPredicate unpackStr (>=)),
    ("string?", applyUnaryOp isString)
  ]

isString :: LispVal -> LispVal
isString (Atom _) = Bool True
isString _ = Bool False