module Evaluators.Primitives.Primitives (primitives, Evaluators.Primitives.Primitives.lookup, PrimitiveCallable) where

import Ast (LispVal (Atom, Bool, DottedList, List, Number, String, args), PrimitiveFunctionKind (Binary, Unary))
import Control.Monad.Except (throwError)
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
import GHC.Float (int2Double)
import LispError (LispError (DivideByZero, NumArgs))

type PrimitiveCallable = [LispVal] -> EvalMonad LispVal

primitives :: Map.Map String PrimitiveCallable
primitives =
  Map.fromList
    ( [ ("-", minus),
        ("+", liftNumericBinOp (+)),
        ("*", liftNumericBinOp (*)),
        ("/", divide),
        ("mod", liftNumericBinOp modulo),
        ("quotient", liftNumericBinOp quotient),
        ("remainder", liftNumericBinOp remainder),
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

minus :: [LispVal] -> EvalMonad LispVal
minus [x] = do
  x' <- unpackNum x
  return (Number (- x'))
minus xs = liftNumericBinOp (-) xs

divide :: [LispVal] -> EvalMonad LispVal
divide xs = do
  if Number 0 `notElem` xs
    then liftNumericBinOp (/) xs
    else throwError (DivideByZero (List xs))

modulo :: Double -> Double -> Double
modulo x y = int2Double $ Prelude.mod (round x) (round y)

quotient :: Double -> Double -> Double
quotient x y = int2Double $ Prelude.mod (round x) (round y)

remainder :: Double -> Double -> Double
remainder x y = int2Double $ Prelude.rem (round x) (round y)

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