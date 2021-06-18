{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Evaluator (performEvalEmpty, LispError (..)) where

import Ast (LispVal (..))
import Control.Monad
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.Reader (MonadReader (ask, local), asks, runReader)
import Data.Functor ((<&>))
import qualified Data.List as List (reverse)
import qualified Data.Map as Map (Map, empty, fromList, lookup, map)
import EvalMonad (EvalMonad (..), VariablesEnv (..))
import Evaluators.ExpToolkit
  ( applyNumericBinOp,
    applyPredicate,
    applyUnaryOp,
    unpackBool,
    unpackNum,
    unpackStr,
  )
import Evaluators.ListPrimitives (listPrimitives)
import Evaluators.StringPrimitives (stringPrimitives)
import LispError (LispError (..))
import Text.Parsec
import Utils.List (evens, odds)

dummyVariablesEnv :: VariablesEnv
dummyVariablesEnv = Map.empty

performEvalEmpty :: LispVal -> Either LispError LispVal
performEvalEmpty expr = performEval expr dummyVariablesEnv

performEval :: LispVal -> VariablesEnv -> Either LispError LispVal
performEval expr = runReader $ runExceptT (eval expr)

eval :: LispVal -> EvalMonad LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval x@(Atom name) = do
  result <- asks (Map.lookup name)
  case result of
    Just value -> return value
    Nothing -> throwError (UnboundVar "attempted to retrieve unbound variable" name)
eval (List [Atom "quote", val]) = return val
eval (List [Atom "unquote", val]) = eval val
eval (List [Atom "if", pred, conseq, alt]) = do
  evaledPred <- eval pred
  case evaledPred of
    (Bool False) -> eval alt
    (Bool True) -> eval conseq
    _ -> throwError (TypeMismatch "bool" evaledPred)
eval (List [Atom "let", List pairs, body]) = do
  varNames <- mapM extractVarName pairs
  varValues <- mapM evalVarBinding pairs
  env <- ask
  let expandedEnv = Map.fromList (zip varNames varValues) <> env
   in local (const expandedEnv) (eval body)
  where
    evalVarBinding :: LispVal -> EvalMonad LispVal
    evalVarBinding (List [varName, varBindExpr]) = eval varBindExpr
    evalVarBinding expr = throwError $ BadSpecialForm "ill-formed let pair expression: " expr
    extractVarName :: LispVal -> EvalMonad String
    extractVarName expr@(List [varName, varBindExpr]) =
      case varName of
        (Atom name) -> return name
        _ -> throwError $ BadSpecialForm "ill-formed let pair expression: " expr
    extractVarName expr = throwError $ BadSpecialForm "ill-formed let pair expression: " expr
eval expr@(List [Atom "if", pred, conseq]) = do
  evaledPred <- eval pred
  case evaledPred of
    (Bool True) -> eval conseq
    (Bool False) -> throwError (BadSpecialForm "no viable alternative in if clause: " expr)
    _ -> throwError (TypeMismatch "bool" evaledPred)
eval (List (Atom "cond" : clauses)) = cond clauses
eval expr@(List (Atom "case" : key : clauses)) = do
  if null clauses
    then throwError $ BadSpecialForm "no viable clause in case expression: " expr
    else case head clauses of
      List (Atom "else" : exprs) -> mapM eval exprs <&> last
      List ((List datum) : exprs) -> do
        evaledKey <- eval key
        matches <- mapM (\x -> eqv [evaledKey, x]) datum
        if Bool True `elem` matches
          then mapM eval exprs <&> last
          else return eval expr (List (Atom "case" : key : tail clauses))
      _ -> throwError $ BadSpecialForm "ill-formed case expression: " expr
eval (List (Atom "begin" : exprs)) = mapM eval exprs <&> last
eval (List (Atom func : args)) = do
  case Map.lookup func primitives of
    (Just funcImpl) -> do
      evaledArgs <- mapM eval args
      apply funcImpl evaledArgs
    Nothing -> throwError (NotFunction "Unrecognised primitive function " func)
eval badForm = throwError (BadSpecialForm "Unrecognized special form " badForm)

apply :: ([LispVal] -> EvalMonad LispVal) -> [LispVal] -> EvalMonad LispVal
apply func = func

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

eqv :: [LispVal] -> EvalMonad LispVal
eqv [(Bool arg1), (Bool arg2)] = return (Bool (arg1 == arg2))
eqv [(Number arg1), (Number arg2)] = return (Bool (arg1 == arg2))
eqv [(String arg1), (String arg2)] = return (Bool (arg1 == arg2))
eqv [(Atom arg1), (Atom arg2)] = return (Bool (arg1 == arg2))
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List (xs ++ [x]), List (ys ++ [y])]
eqv [(List xs), (List ys)] =
  if length xs /= length ys
    then return (Bool False)
    else do
      pairsEq <- mapM eqv [[x, y] | (x, y) <- zip xs ys]
      allPairsEq <- mapM unpackBool pairsEq
      return (Bool (and allPairsEq))

cond :: [LispVal] -> EvalMonad LispVal
cond [List [Atom "else", value]] = eval value
cond ((List [condition, value]) : alts) = do
  condPred <- eval condition
  case condPred of
    (Bool True) -> eval value
    (Bool False) -> cond alts
    _ -> throwError $ Default "Cond predicate evaluated to a non bool value."
cond ((List a) : _) = throwError $ NumArgs 2 a
cond (a : _) = throwError $ NumArgs 2 [a]
cond _ = throwError $ Default "Not viable alternative in cond"