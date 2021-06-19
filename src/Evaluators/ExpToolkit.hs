module Evaluators.ExpToolkit
  ( NumericBinOp (..),
    UnaryOp (..),
    PredicateBinOp (..),
    applyUnaryOp,
    applyNumericBinOp,
    applyPredicate,
    unpackNum,
    unpackStr,
    unpackBool,
  )
where

import Ast (LispVal (Bool, Number, String))
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError))
import Control.Monad.Reader (Reader)
import Data.Functor ((<&>))
import EvalMonad (EvalMonad, EvaluationEnv)
import LispError (LispError (NumArgs, TypeMismatch))

type NumericBinOp = (Integer -> Integer -> Integer)

type UnaryOp = (LispVal -> LispVal)

type PredicateBinOp a = (a -> a -> Bool)

applyNumericBinOp :: NumericBinOp -> [LispVal] -> EvalMonad LispVal
applyNumericBinOp op args = mapM unpackNum args <&> Number . foldl1 op

applyUnaryOp :: UnaryOp -> [LispVal] -> EvalMonad LispVal
applyUnaryOp op args = case args of
  [arg] -> return (op arg)
  _ -> throwError (NumArgs 2 args)

applyPredicate :: (LispVal -> EvalMonad a) -> PredicateBinOp a -> [LispVal] -> EvalMonad LispVal
applyPredicate unpack op args
  | length args /= 2 = throwError (NumArgs 2 args)
  | otherwise = do
    left <- unpack $ head args
    right <- unpack $ (head . tail) args
    return (Bool (left `op` right))

unpackNum :: LispVal -> EvalMonad Integer
unpackNum (Number x) = return x
unpackNum notNum = throwError (TypeMismatch "number" notNum)

unpackStr :: LispVal -> EvalMonad String
unpackStr (String s) = return s
unpackStr notStr = throwError (TypeMismatch "string" notStr)

unpackBool :: LispVal -> EvalMonad Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError (TypeMismatch "bool" notBool)