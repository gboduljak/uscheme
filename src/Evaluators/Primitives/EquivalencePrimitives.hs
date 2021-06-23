module Evaluators.Primitives.EquivalencePrimitives where

import Ast (LispVal (Atom, Bool, DottedList, List, Number, String))
import Control.Monad.Except
import EvalMonad (EvalMonad)
import Evaluators.Toolkits.ExpToolkit (unpackBool)
import LispError

eqv :: [LispVal] -> EvalMonad LispVal
eqv [Bool x, Bool y] = return . Bool $ x == y
eqv [Number x, Number y] = return . Bool $ x == y
eqv [String x, String y] = return . Bool $ x == y
eqv [Atom x, Atom y] = return . Bool $ x == y
eqv [DottedList xs x, DottedList ys y] = eqv [List (xs ++ [x]), List (ys ++ [y])]
eqv [List xs, List ys] =
  if length xs /= length ys
    then return (Bool False)
    else do
      pairsEq <- mapM eqv [[x, y] | (x, y) <- zip xs ys]
      allPairsEq <- mapM unpackBool pairsEq
      return (Bool (and allPairsEq))
eqv [_, _] = return (Bool False)
eqv args = throwError (NumArgs 2 args)