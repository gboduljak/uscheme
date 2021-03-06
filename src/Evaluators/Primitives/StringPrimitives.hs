{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Evaluators.Primitives.StringPrimitives (stringPrimitives) where

import Ast (LispVal (Atom, Bool))
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError))
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import EvalMonad (EvalMonad)
import Evaluators.Toolkits.ExpToolkit (liftLogicalBinOp, liftUnaryOp, unpackStr)
import LispError (LispError (Default, NumArgs, TypeMismatch))

stringPrimitives :: [(String, [LispVal] -> EvalMonad LispVal)]
stringPrimitives =
  [ ("string=?", liftLogicalBinOp unpackStr (==)),
    ("string<?", liftLogicalBinOp unpackStr (<)),
    ("string>?", liftLogicalBinOp unpackStr (>)),
    ("string<=?", liftLogicalBinOp unpackStr (<=)),
    ("string>=?", liftLogicalBinOp unpackStr (>=)),
    ("string?", liftUnaryOp isString)
  ]

isString :: LispVal -> LispVal
isString (Atom _) = Bool True
isString _ = Bool False