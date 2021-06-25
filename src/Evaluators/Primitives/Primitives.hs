module Evaluators.Primitives.Primitives (primitives, Evaluators.Primitives.Primitives.lookup, asBool, and, or) where

import Ast (LispVal (Atom, Bool, DottedList, List, Number, String, args))
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
import Prelude hiding (and, or)

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
        ("&&", logicalAndOp),
        ("||", logicalOrOp),
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

logicalOrOp :: [LispVal] -> EvalMonad LispVal
logicalOrOp [x, y] = do
  x' <- asBool x
  if x'
    then return x
    else do
      y' <- asBool y
      if y'
        then return y
        else return (Bool False)
logicalOrOp expr = throwError (NumArgs 2 expr)

logicalAndOp :: [LispVal] -> EvalMonad LispVal
logicalAndOp [x, y] = do
  x' <- asBool x
  if not x'
    then return (Bool False)
    else do
      y' <- asBool y
      if y'
        then return y
        else return (Bool False)
logicalAndOp expr = throwError (NumArgs 2 expr)

and :: [LispVal] -> (LispVal -> EvalMonad LispVal) -> EvalMonad LispVal
and [] _ = return (Bool True)
and [x] eval = do
  x' <- eval x
  x'' <- asBool x'
  if x''
    then return x'
    else return (Bool False)
and (x : xs) eval = do
  x' <- eval x
  x'' <- asBool x'
  if x''
    then and xs eval
    else return x'

or :: [LispVal] -> (LispVal -> EvalMonad LispVal) -> EvalMonad LispVal
or [] _ = return (Bool False)
or [x] eval = do
  x' <- eval x
  x'' <- asBool x'
  if x''
    then return x'
    else return (Bool False)
or (x : xs) eval = do
  x' <- eval x
  x'' <- asBool x'
  if x''
    then return x'
    else or xs eval

asBool :: LispVal -> EvalMonad Bool
asBool (Bool x) = return x
asBool _ = return True

isSymbol :: LispVal -> LispVal
isSymbol (Atom _) = Bool True
isSymbol _ = Bool False

isNumber :: LispVal -> LispVal
isNumber (Number _) = Bool True
isNumber _ = Bool False

isBool :: LispVal -> LispVal
isBool (Bool _) = Bool True
isBool _ = Bool False