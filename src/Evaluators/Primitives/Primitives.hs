module Evaluators.Primitives.Primitives (primitives, Evaluators.Primitives.Primitives.lookup, PrimitiveCallable) where

import Ast (LispVal (Atom, Bool, DottedList, List, Number, String), PrimitiveFunctionKind (Binary, Unary))
import qualified Data.Map as Map
import EvalMonad
import Evaluators.Primitives.EquivalencePrimitives (eqv)
import Evaluators.Primitives.ListPrimitives (listPrimitives)
import Evaluators.Primitives.StringPrimitives (stringPrimitives)
import Evaluators.Toolkits.ExpToolkit
  ( liftLogicalBinOp,
    liftNumericBinOp,
    liftUnaryOp,
    unpackBool,
    unpackNum,
  )

type PrimitiveCallable = [LispVal] -> EvalMonad LispVal

primitives :: Map.Map String PrimitiveCallable
primitives =
  Map.fromList
    ( [ ("+", liftNumericBinOp (+)),
        ("-", liftNumericBinOp (-)),
        ("*", liftNumericBinOp (*)),
        ("/", liftNumericBinOp div),
        ("mod", liftNumericBinOp mod),
        ("quotient", liftNumericBinOp quot),
        ("remainder", liftNumericBinOp rem),
        ("symbol?", liftUnaryOp isSymbol),
        ("number?", liftUnaryOp isNumber),
        ("bool?", liftUnaryOp isBool),
        ("=", liftLogicalBinOp unpackNum (==)),
        ("<", liftLogicalBinOp unpackNum (<)),
        (">", liftLogicalBinOp unpackNum (>)),
        ("<=", liftLogicalBinOp unpackNum (<=)),
        (">=", liftLogicalBinOp unpackNum (>=)),
        ("/=", liftLogicalBinOp unpackNum (/=)),
        ("&&", liftLogicalBinOp unpackBool (&&)),
        ("||", liftLogicalBinOp unpackBool (||)),
        ("eqv?", eqv),
        ("eq?", eqv),
        ("equal?", eqv)
      ]
        ++ listPrimitives
        ++ stringPrimitives
    )

lookup :: String -> Maybe PrimitiveCallable
lookup name = Map.lookup name primitives

isSymbol :: LispVal -> LispVal
isSymbol (Atom _) = Bool True
isSymbol _ = Bool False

isNumber :: LispVal -> LispVal
isNumber (Number _) = Bool True
isNumber _ = Bool False

isBool :: LispVal -> LispVal
isBool (Bool _) = Bool True
isBool _ = Bool False