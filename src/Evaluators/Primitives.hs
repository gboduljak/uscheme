module Evaluators.Primitives (primitives) where

import Ast (LispVal (Atom, Bool, DottedList, List, Number, String))
import qualified Data.Map as Map
import EvalMonad
import Evaluators.ExpToolkit
  ( applyNumericBinOp,
    applyPredicate,
    applyUnaryOp,
    unpackBool,
    unpackNum,
  )
import Evaluators.ListPrimitives (listPrimitives)
import Evaluators.StringPrimitives (stringPrimitives)

primitives :: Map.Map String ([LispVal] -> EvalMonad LispVal)
primitives =
  Map.fromList
    ( [ ("+", applyNumericBinOp (+)),
        ("-", applyNumericBinOp (-)),
        ("*", applyNumericBinOp (*)),
        ("/", applyNumericBinOp div),
        ("mod", applyNumericBinOp mod),
        ("quotient", applyNumericBinOp quot),
        ("remainder", applyNumericBinOp rem),
        ("symbol?", applyUnaryOp isSymbol),
        ("number?", applyUnaryOp isNumber),
        ("bool?", applyUnaryOp isBool),
        ("=", applyPredicate unpackNum (==)),
        ("<", applyPredicate unpackNum (<)),
        (">", applyPredicate unpackNum (>)),
        ("<=", applyPredicate unpackNum (<=)),
        (">=", applyPredicate unpackNum (>=)),
        ("/=", applyPredicate unpackNum (/=)),
        ("&&", applyPredicate unpackBool (&&)),
        ("||", applyPredicate unpackBool (||)),
        ("eqv?", eqv),
        ("eq?", eqv),
        ("equal?", eqv)
      ]
        ++ listPrimitives
        ++ stringPrimitives
    )

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

isSymbol :: LispVal -> LispVal
isSymbol (Atom _) = Bool True
isSymbol _ = Bool False

isNumber :: LispVal -> LispVal
isNumber (Number _) = Bool True
isNumber _ = Bool False

isBool :: LispVal -> LispVal
isBool (Bool _) = Bool True
isBool _ = Bool False