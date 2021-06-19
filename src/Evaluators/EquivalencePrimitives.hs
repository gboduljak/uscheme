module Evaluators.EquivalencePrimitives where

import Ast (LispVal (Atom, Bool, DottedList, List, Number, String))
import EvalMonad (EvalMonad)
import Evaluators.ExpToolkit (unpackBool)

eqv :: [LispVal] -> EvalMonad LispVal
eqv [Bool arg1, Bool arg2] = return (Bool (arg1 == arg2))
eqv [Number arg1, Number arg2] = return (Bool (arg1 == arg2))
eqv [String arg1, String arg2] = return (Bool (arg1 == arg2))
eqv [Atom arg1, Atom arg2] = return (Bool (arg1 == arg2))
eqv [DottedList xs x, DottedList ys y] = eqv [List (xs ++ [x]), List (ys ++ [y])]
eqv [List xs, List ys] =
  if length xs /= length ys
    then return (Bool False)
    else do
      pairsEq <- mapM eqv [[x, y] | (x, y) <- zip xs ys]
      allPairsEq <- mapM unpackBool pairsEq
      return (Bool (and allPairsEq))
eqv _ = return (Bool False)