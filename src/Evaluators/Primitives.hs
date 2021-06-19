module Evaluators.Primitives (primitives) where

import Ast (LispVal (Atom, Bool, DottedList, List, Number, String))
import qualified Data.Map as Map
import EvalMonad
import Evaluators.EquivalencePrimitives (eqv)
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

isSymbol :: LispVal -> LispVal
isSymbol (Atom _) = Bool True
isSymbol _ = Bool False

isNumber :: LispVal -> LispVal
isNumber (Number _) = Bool True
isNumber _ = Bool False

isBool :: LispVal -> LispVal
isBool (Bool _) = Bool True
isBool _ = Bool False