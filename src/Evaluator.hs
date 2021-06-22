{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Evaluator
  ( evaluateOnEmptyContext,
    evaluate,
    evaluateMany,
    evaluateManyParallel,
  )
where

import Ast (LispVal (..))
import Control.Monad (foldM)
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError), runExcept, runExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (MonadReader (ask, local), Reader, asks, runReader)
import Control.Monad.State (MonadState (get, put), StateT (runStateT), gets)
import Data.Functor (($>), (<&>))
import qualified Data.List as List (reverse)
import qualified Data.Map as Map (Map, empty, fromList, insert, lookup, map)
import EvalMonad (EvalMonad (..))
import qualified Evaluators.Application as Application (eval)
import qualified Evaluators.Atom as Atom (eval)
import qualified Evaluators.Define as Define (eval)
import qualified Evaluators.Lambda as Lambda (eval)
import qualified Evaluators.Let as Let (eval)
import Evaluators.Primitives.EquivalencePrimitives (eqv)
import Evaluators.Primitives.Primitives (primitives)
import qualified Evaluators.Set as Set
import LispError (LispError (..))
import Scoping.ScopeResolver
  ( ScopeContext (ScopeContext),
    getInitialScopeContext,
    runScopeResolver,
  )

evaluateMany :: [LispVal] -> ScopeContext -> (Either LispError LispVal, ScopeContext)
evaluateMany exprs initCtx =
  foldl
    ( \acc exp -> case acc of
        error@(Left _, _) -> error
        (Right value, ctx) -> evaluate exp ctx
    )
    (Right (Atom "init"), initCtx)
    exprs

evaluateManyParallel :: [LispVal] -> ScopeContext -> [(Either LispError LispVal, ScopeContext)]
evaluateManyParallel exprs initCtx = map (`evaluate` initCtx) exprs

evaluate :: LispVal -> ScopeContext -> (Either LispError LispVal, ScopeContext)
evaluate expr = runIdentity . runStateT (runExceptT (eval expr))

evaluateOnEmptyContext :: LispVal -> (Either LispError LispVal, ScopeContext)
evaluateOnEmptyContext expr = evaluate expr getInitialScopeContext

eval :: LispVal -> EvalMonad LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval expr@(Atom "nil") = return (List [])
eval expr@(Atom name) = Atom.eval expr
eval (List [Atom "quote", val]) = return val
eval (List [Atom "unquote", val]) = eval val
eval (List [Atom "if", pred, conseq, alt]) = do
  evaledPred <- eval pred
  case evaledPred of
    (Bool False) -> eval alt
    (Bool True) -> eval conseq
    _ -> throwError (TypeMismatch "bool" evaledPred)
eval (List (Atom "begin" : rest)) = last <$> mapM eval rest
eval expr@(List [Atom "let", List pairs, body]) = Let.eval expr eval
eval expr@(List [Atom "define", DottedList args arg, funcBody]) = Define.eval expr eval
eval expr@(List [Atom "define", List (funcNameExpr : funcArgs), funcBody]) = Define.eval expr eval
eval expr@(List (Atom "define" : List (funcNameExpr : funcArgs) : funcBody)) = Define.eval expr eval
eval expr@(List [Atom "define", funcNameExpr, bindingValExpr]) = Define.eval expr eval
eval expr@(List [Atom "set!", bindingName, bindingValueExpr]) = Set.eval expr eval
eval expr@(List (Atom "lambda" : List args : body)) = Lambda.eval expr
eval expr@(List (Atom "lambda" : DottedList args (Atom varargs) : body)) = Lambda.eval expr
eval expr@(List (Atom "lambda" : (Atom varargs) : body)) = Lambda.eval expr
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
eval expr@(List (head : args)) = Application.eval expr eval
eval expr = throwError (BadSpecialForm "unrecognized special form" expr)

cond :: [LispVal] -> EvalMonad LispVal
cond [List [Atom "else", value]] = eval value
cond ((List [condition, value]) : alts) = do
  condPred <- eval condition
  case condPred of
    (Bool True) -> eval value
    (Bool False) -> cond alts
    _ -> throwError $ Default "cond predicate evaluated to a non bool value."
cond ((List a) : _) = throwError $ NumArgs 2 a
cond (a : _) = throwError $ NumArgs 2 [a]
cond _ = throwError $ Default "not viable alternative in cond"