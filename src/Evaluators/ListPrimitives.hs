module Evaluators.ListPrimitives (listPrimitives) where

import Ast (LispVal (Bool, DottedList, List))
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError))
import Control.Monad.Reader (Reader)
import qualified Data.List as List
import EvalMonad (EvalMonad, EvaluationEnv)
import Evaluators.ExpToolkit (applyUnaryOp)
import LispError (LispError (Default, NumArgs, TypeMismatch))

listPrimitives :: [(String, [LispVal] -> EvalMonad LispVal)]
listPrimitives =
  [ ("list?", applyUnaryOp isList),
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("reverse", Evaluators.ListPrimitives.reverse)
  ]

isList :: LispVal -> LispVal
isList (List _) = Bool True
isList (DottedList _ _) = Bool True
isList _ = Bool False

car :: [LispVal] -> EvalMonad LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> EvalMonad LispVal
cdr [List (x : xs)] = return (List xs)
cdr [DottedList [xs] x] = return x
cdr [DottedList (_ : xs) x] = return (DottedList xs x)
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr args = throwError $ NumArgs 1 args

cons :: [LispVal] -> EvalMonad LispVal
cons [x, List []] = return (List [x])
cons [x, List xs] = return (List (x : xs))
cons [x, DottedList ys y] = return (DottedList (x : ys) y)
cons [x1, x2] = return (DottedList [x1] x2)
cons args = throwError $ NumArgs 2 args

reverse :: [LispVal] -> EvalMonad LispVal
reverse [x] = case x of
  List xs -> return (List (List.reverse xs))
  badType -> throwError (Default "tried to reverse a non list value")
reverse args = throwError (NumArgs 1 args)