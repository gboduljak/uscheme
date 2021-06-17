{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Evaluator (performEvalEmpty, LispError (..)) where

import Ast (LispVal (..))
import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    runExceptT,
  )
import Control.Monad.Reader
import Data.Functor
import qualified Data.Map as Map (Map, empty, fromList, lookup, map)
import Text.Parsec

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | ParserError ParseError
  | Default String
  deriving (Eq)

instance Show LispError where
  show (NumArgs expected found) = "Expected " ++ show expected ++ " args: found values " ++ unwords (map show found)
  show (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ " , found " ++ show found
  show (BadSpecialForm message form) = message ++ " : " ++ show form
  show (NotFunction message func) = message ++ " : " ++ show func
  show (UnboundVar message var) = message ++ " : " ++ var
  show (ParserError error) = show error
  show (Default error) = error

type VariablesEnv = Map.Map String LispVal

type EvalWithEnvAndExcept a = ExceptT LispError (Reader VariablesEnv) a

dummyVariablesEnv :: VariablesEnv
dummyVariablesEnv = Map.empty

performEvalEmpty :: LispVal -> Either LispError LispVal
performEvalEmpty expr = performEval expr dummyVariablesEnv

performEval :: LispVal -> VariablesEnv -> Either LispError LispVal
performEval expr = runReader $ runExceptT (eval expr)

type NumericBinOp = (Integer -> Integer -> Integer)

type LispValUnaryOp = (LispVal -> LispVal)

eval :: LispVal -> EvalWithEnvAndExcept LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "unquote", val]) = eval val
eval (List (Atom func : args)) = do
  case Map.lookup func primitives of
    (Just funcImpl) -> do
      evaledArgs <- mapM eval args
      apply funcImpl evaledArgs
    Nothing -> throwError (NotFunction "Unrecognised primitive function " func)
eval badForm = throwError (BadSpecialForm "Unrecognized special form " badForm)

apply :: ([LispVal] -> EvalWithEnvAndExcept LispVal) -> [LispVal] -> EvalWithEnvAndExcept LispVal
apply func = func

primitives :: Map.Map String ([LispVal] -> EvalWithEnvAndExcept LispVal)
primitives =
  Map.fromList
    [ ("+", applyNumericBinOp (+)),
      ("-", applyNumericBinOp (-)),
      ("*", applyNumericBinOp (*)),
      ("/", applyNumericBinOp div),
      ("mod", applyNumericBinOp mod),
      ("quotient", applyNumericBinOp quot),
      ("remainder", applyNumericBinOp rem),
      ("symbol?", applyLispValUnaryOp isSymbol),
      ("string?", applyLispValUnaryOp isString),
      ("number?", applyLispValUnaryOp isNumber),
      ("bool?", applyLispValUnaryOp isBool),
      ("list?", applyLispValUnaryOp isList)
    ]

isSymbol :: LispVal -> LispVal
isSymbol (Atom _) = Bool True
isSymbol _ = Bool False

isString :: LispVal -> LispVal
isString (Atom _) = Bool True
isString _ = Bool False

isNumber :: LispVal -> LispVal
isNumber (Number _) = Bool True
isNumber _ = Bool False

isBool :: LispVal -> LispVal
isBool (Bool _) = Bool True
isBool _ = Bool False

isList :: LispVal -> LispVal
isList (List _) = Bool True
isList (DottedList _ _) = Bool True
isList _ = Bool False

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> EvalWithEnvAndExcept LispVal
unaryOp f [v] = return (f v)
unaryOp f args = throwError (NumArgs 1 args)

applyNumericBinOp :: NumericBinOp -> [LispVal] -> EvalWithEnvAndExcept LispVal
applyNumericBinOp op args = mapM unpackNum args <&> Number . foldl1 op
  where
    unpackNum :: LispVal -> EvalWithEnvAndExcept Integer
    unpackNum (Number x) = return x
    unpackNum notNum = throwError (TypeMismatch "number" notNum)

applyLispValUnaryOp :: LispValUnaryOp -> [LispVal] -> EvalWithEnvAndExcept LispVal
applyLispValUnaryOp = undefined