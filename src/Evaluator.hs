{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Evaluator (performEvalEmpty, performEval, LispError (..)) where

import Ast (LispVal (..))
import Control.Monad
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError), runExceptT)
import Control.Monad.Reader (MonadReader (ask, local), Reader, asks, runReader)
import Control.Monad.State (MonadState (get, put), StateT (runStateT), gets)
import Data.Functor (($>), (<&>))
import qualified Data.List as List (reverse)
import qualified Data.Map as Map (Map, empty, fromList, insert, lookup, map)
import EvalMonad (EvalMonad (..), EvaluationEnv (..), EvaluationState (..), unusedLambdaId)
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

dummyEvaluationEnv :: EvaluationEnv
dummyEvaluationEnv = Env {variables = Map.empty, isGlobal = True}

dummyState :: EvaluationState
dummyState =
  St
    { globalEnv = dummyEvaluationEnv,
      lambdaContexts = Map.empty
    }

performEvalEmpty :: LispVal -> Either LispError LispVal
performEvalEmpty expr = case performEval expr dummyEvaluationEnv dummyState of
  Left e -> Left e
  Right (val, st) -> Right val

performEval :: LispVal -> EvaluationEnv -> EvaluationState -> Either LispError (LispVal, EvaluationState)
performEval expr env state = runReader (runExceptT (runStateT (eval expr) state)) env

eval :: LispVal -> EvalMonad LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (Atom name) = do
  result <- asks (Map.lookup name . variables)
  case result of
    Just value -> return value
    _ -> throwError (UnboundVar "attempted to retrieve unbound variable" name)
eval (List [Atom "quote", val]) = return val
eval (List [Atom "unquote", val]) = eval val
eval (List [Atom "if", pred, conseq, alt]) = do
  evaledPred <- eval pred
  case evaledPred of
    (Bool False) -> eval alt
    (Bool True) -> eval conseq
    _ -> throwError (TypeMismatch "bool" evaledPred)
eval (List [Atom "let", List pairs, body]) = do
  varNames <- mapM extractPairVarName pairs
  varValues <- mapM evalPairVarBinding pairs
  env <- ask
  let expandedEnv =
        Env
          { variables = Map.fromList (zip varNames varValues) <> variables env,
            isGlobal = False
          }
   in local (const expandedEnv) (eval body)
  where
    evalPairVarBinding :: LispVal -> EvalMonad LispVal
    evalPairVarBinding (List [varName, varBindExpr]) = eval varBindExpr
    evalPairVarBinding expr = throwError $ BadSpecialForm "ill-formed let pair expression: " expr
    extractPairVarName :: LispVal -> EvalMonad String
    extractPairVarName expr@(List [varName, varBindExpr]) =
      case varName of
        (Atom name) -> return name
        _ -> throwError $ BadSpecialForm "ill-formed let pair expression: " expr
    extractPairVarName expr = throwError $ BadSpecialForm "ill-formed let pair expression: " expr
eval (List [Atom "define", List (name : params), body]) = do
  funcName <- unpackAtomId name
  builtFunc <- buildFunction funcName params body
  defineAndRunScoped name (List [Atom "quote", builtFunc]) (return name)
eval (List [Atom "define", DottedList params param, body]) = do
  let name = head params
   in do
        funcName <- unpackAtomId name
        builtFunc <- buildFunction funcName (tail params ++ [param]) body
        defineAndRunScoped (head params) (List [Atom "quote", builtFunc]) (return name)
eval expr@(List [Atom "define", name, bodyExpr]) = defineAndRunScoped name bodyExpr (return name)
eval (List [Atom "begin", rest]) = evalBody rest
eval (List ((:) (Atom "begin") rest)) = evalBody $ List rest
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
eval (List [Atom "lambda", List lambdaArgs, lambdaBody]) = buildLambda lambdaArgs lambdaBody
eval expr@(List (Atom "lambda" : _)) = throwError $ BadSpecialForm "ill-formed lambda expression: " expr
eval expr@(List (head : args)) = do
  vars <- asks variables
  case head of
    (Atom name) -> case Map.lookup name vars of
      (Just lambda) -> applyLambda lambda args
      Nothing -> applyPrimitive name args
    _ ->
      eval head
        >>= ( \case
                lambda@Lambda {} -> applyLambda lambda args
                _ -> throwError (BadSpecialForm "Unrecognized special form: " expr)
            )
  where
    applyPrimitive func args = do
      case Map.lookup func primitives of
        (Just funcImpl) -> do
          evaledArgs <- mapM eval args
          apply funcImpl evaledArgs
        Nothing -> throwError (NotFunction "Unrecognised primitive function " func)
    applyLambda lambda@(Lambda id args body) argsToBind = do
      env <- ask
      lambdaCtxts <- gets lambdaContexts
      evaledArgs <- mapM eval argsToBind
      case Map.lookup id lambdaCtxts of
        (Just lambdaEnv) -> do
          let lambdaEvalEnv = Env {variables = Map.fromList (zip args evaledArgs) <> variables lambdaEnv, isGlobal = False}
           in local (const lambdaEvalEnv) (eval body)
        _ -> throwError (BadSpecialForm "Tried to evaluate unrecognised lambda: " lambda)
    applyLambda xs _ = throwError (BadSpecialForm "Unrecognized special form " xs)
eval badForm = throwError (BadSpecialForm "Unrecognized special form " badForm)

buildFunction :: String -> [LispVal] -> LispVal -> EvalMonad LispVal
buildFunction name lambdaArgs lambdaBody = do
  env <- ask
  state <- get
  args <- mapM unpackAtomId lambdaArgs
  let lambdaId = unusedLambdaId state
   in let lambda = Lambda {lambdaId = lambdaId, args = args, body = lambdaBody}
       in put
            St
              { globalEnv = globalEnv state,
                lambdaContexts =
                  Map.insert
                    lambdaId
                    ( Env
                        { variables = Map.insert name lambda (variables env),
                          isGlobal = isGlobal env
                        }
                    )
                    (lambdaContexts state)
              }
            $> Lambda {lambdaId = lambdaId, args = args, body = lambdaBody}

buildLambda :: [LispVal] -> LispVal -> EvalMonad LispVal
buildLambda lambdaArgs lambdaBody = do
  env <- ask
  state <- get
  args <- mapM unpackAtomId lambdaArgs
  let lambdaId = unusedLambdaId state
   in put
        St
          { globalEnv = globalEnv state,
            lambdaContexts = Map.insert lambdaId env (lambdaContexts state)
          }
        $> Lambda {lambdaId = lambdaId, args = args, body = lambdaBody}
  where
    unpackAtomId :: LispVal -> EvalMonad String
    unpackAtomId (Atom x) = return x
    unpackAtomId val = throwError (BadSpecialForm "tried to define lambda with arg: " val)

defineAndRunScoped :: LispVal -> LispVal -> EvalMonad LispVal -> EvalMonad LispVal
defineAndRunScoped defineExpr definitionExpr scopedExpr = case defineExpr of
  (Atom name) -> do
    do
      evaledExpr <- eval definitionExpr
      env <- ask
      let boundEnv =
            Env
              { variables = Map.insert name evaledExpr (variables env),
                isGlobal = isGlobal env
              }
       in do
            if isGlobal env
              then do
                globalVars <- gets (variables . globalEnv)
                globalLambdaCtx <- gets lambdaContexts
                put
                  ( St
                      { globalEnv =
                          Env
                            { variables = Map.insert name evaledExpr globalVars,
                              isGlobal = True
                            },
                        lambdaContexts = globalLambdaCtx
                      }
                  )
                local (const boundEnv) scopedExpr
              else local (const boundEnv) scopedExpr
  _ -> throwError $ BadSpecialForm "ill-formed define expression: " defineExpr

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

evalBody :: LispVal -> EvalMonad LispVal
evalBody (List [List ((Atom "define") : [Atom var, defExpr]), rest]) = defineAndRunScoped (Atom var) defExpr (eval rest)
evalBody (List ((List ((Atom "define") : [Atom var, defExpr])) : rest)) = defineAndRunScoped (Atom var) defExpr (evalBody (List rest))
evalBody (List [List [Atom "define", List (name : params), body], rest]) = do
  funcName <- unpackAtomId name
  builtFunc <- buildFunction funcName params body
  defineAndRunScoped name (List [Atom "quote", builtFunc]) (eval rest)
evalBody (List [List [Atom "define", DottedList params param, body], rest]) = do
  let name = head params
   in do
        funcName <- unpackAtomId name
        builtFunc <- buildFunction funcName (tail params ++ [param]) body
        defineAndRunScoped (head params) (List [Atom "quote", builtFunc]) (eval rest)
evalBody (List ((List [Atom "define", List (name : params), body]) : rest)) = do
  funcName <- unpackAtomId name
  builtFunc <- buildFunction funcName params body
  defineAndRunScoped name (List [Atom "quote", builtFunc]) (evalBody (List rest))
evalBody (List ((List [Atom "define", DottedList params param, body]) : rest)) = do
  let name = head params
   in do
        funcName <- unpackAtomId name
        builtFunc <- buildFunction funcName (tail params ++ [param]) body
        defineAndRunScoped (head params) (List [Atom "quote", builtFunc]) (evalBody (List rest))

isSymbol :: LispVal -> LispVal
isSymbol (Atom _) = Bool True
isSymbol _ = Bool False

isNumber :: LispVal -> LispVal
isNumber (Number _) = Bool True
isNumber _ = Bool False

isBool :: LispVal -> LispVal
isBool (Bool _) = Bool True
isBool _ = Bool False

unpackAtomId :: LispVal -> EvalMonad String
unpackAtomId (Atom x) = return x
unpackAtomId val = throwError (BadSpecialForm "tried to define lambda with arg: " val)

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