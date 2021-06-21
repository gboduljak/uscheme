module Evaluators.Toolkits.ExpToolkit
  ( NumericBinOp (..),
    UnaryOp (..),
    PredicateBinOp (..),
    liftUnaryOp,
    liftNumericBinOp,
    liftLogicalBinOp,
    unpackNum,
    unpackStr,
    unpackBool,
    unpackAtomValue,
  )
where

import Ast (LispVal (Atom, Bool, Number, String))
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError))
import Control.Monad.Reader (Reader)
import Data.Functor ((<&>))
import EvalMonad (EvalMonad)
import LispError (LispError (NumArgs, TypeMismatch))

type NumericBinOp = (Double -> Double -> Double)

type UnaryOp = (LispVal -> LispVal)

type PredicateBinOp a = (a -> a -> Bool)

liftNumericBinOp :: NumericBinOp -> [LispVal] -> EvalMonad LispVal
liftNumericBinOp op args = mapM unpackNum args <&> Number . foldl1 op

liftUnaryOp :: UnaryOp -> [LispVal] -> EvalMonad LispVal
liftUnaryOp op args = case args of
  [arg] -> return (op arg)
  _ -> throwError (NumArgs 1 args)

liftLogicalBinOp :: (LispVal -> EvalMonad a) -> PredicateBinOp a -> [LispVal] -> EvalMonad LispVal
liftLogicalBinOp unpack op args
  | length args /= 2 = throwError (NumArgs 2 args)
  | otherwise = do
    left <- unpack $ head args
    right <- unpack $ (head . tail) args
    return (Bool (left `op` right))

unpackNum :: LispVal -> EvalMonad Double
unpackNum (Number x) = return x
unpackNum notNum = throwError (TypeMismatch "number" notNum)

unpackStr :: LispVal -> EvalMonad String
unpackStr (String s) = return s
unpackStr notStr = throwError (TypeMismatch "string" notStr)

unpackBool :: LispVal -> EvalMonad Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError (TypeMismatch "bool" notBool)

unpackAtomValue :: LispVal -> EvalMonad String
unpackAtomValue (Atom value) = return value
unpackAtomValue notAtom = throwError (TypeMismatch "atom" notAtom)