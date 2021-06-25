{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module MixedSpec (mixedSpec) where

import Ast (LispVal (..))
import Control.Monad.State (Monad (return), MonadIO (liftIO), MonadTrans (lift), gets, runState)
import Data.Either (Either (Left, Right))
import qualified Data.Map as Map (size)
import Evaluator (evaluate, evaluateMany, evaluateManySeq)
import Helpers (fails)
import LispError (LispError (..))
import Parser (expr, parse)
import Scoping.ScopeResolver (getInitialScopeContext)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import qualified Text.ParserCombinators.Parsec as Parsec (ParseError, parse)
import Prelude hiding (id, lookup)

mixedSpec :: Spec
mixedSpec = do
  describe "mixed language features tests..." $ do
    it "correctly evaluates the first sequence of mixed language ops" $ do
      stdLib <- liftIO (readFile "./lib/stdlib.scm")
      input <- liftIO (readFile "./test/tests/berkeley/mixed.scm")
      let emptyCtx = getInitialScopeContext
      case parse stdLib of
        (Right stdlibExps) -> do
          case evaluateMany stdlibExps emptyCtx of
            (Right _, testCtx) -> do
              case parse input of
                (Right tree) -> do
                  let results = map fst $ evaluateManySeq tree testCtx
                  results
                    `shouldBe` [ Right (Number 9.0),
                                 Right (Bool True),
                                 Right (Bool True),
                                 Right (List [Number 1.0, DottedList [Number 2.0, Atom "three"] (DottedList [Number 4.0] (Number 5.0))]),
                                 Right (Lambda {args = ["x"], body = [List [Atom "+", Atom "x", Number 1.0]], targetScopeId = 0, varargs = Nothing}),
                                 Right (Number 3.0),
                                 Right (Lambda {args = ["x"], body = [List [Atom "*", Atom "x", Number 10.0]], targetScopeId = 0, varargs = Nothing}),
                                 Right (Number 50.0),
                                 Right (Atom "sign"),
                                 Right (Atom "+"),
                                 Right (Lambda {args = ["s"], body = [List [Atom "cdr", List [Atom "cdr", Atom "s"]]], targetScopeId = 0, varargs = Nothing}),
                                 Right (Lambda {args = ["s"], body = [List [Atom "car", List [Atom "cdr", Atom "s"]]], targetScopeId = 0, varargs = Nothing}),
                                 Right (Lambda {args = ["s"], body = [List [Atom "car", List [Atom "cddr", Atom "s"]]], targetScopeId = 0, varargs = Nothing}),
                                 Right (Atom "evens"),
                                 Right (List [Number 4.0, Number 6.0, Number 8.0]),
                                 Right (Number 2.0),
                                 Right (Number 4.0),
                                 Right (Number 1.0),
                                 Right (Lambda {args = ["x"], body = [List [Atom "+", Atom "x", Number 1.0]], targetScopeId = 0, varargs = Nothing}),
                                 Right (Lambda {args = ["x"], body = [List [Atom "/", Atom "x", List [Atom "*", Atom "x", Number 2.0]]], targetScopeId = 0, varargs = Nothing}),
                                 Right (Lambda {args = ["n"], body = [List [Atom "if", List [Atom "<", Atom "n", Number 2.0], Number 1.0, List [Atom "+", List [Atom "fib", List [Atom "-", Atom "n", Number 1.0]], List [Atom "fib", List [Atom "-", Atom "n", Number 2.0]]]]], targetScopeId = 0, varargs = Nothing}),
                                 Right (Number 55.0),
                                 Right (Number 1.5),
                                 Right (Atom "scheme!"),
                                 Right (List [Atom "i", Atom "love", Atom "coding", Atom "in", Atom "scheme"]),
                                 Right (Atom "b"),
                                 Right (Number 4.0),
                                 Right (Number 2.0),
                                 Right (Atom "nine"),
                                 Right (Lambda {args = ["x", "y"], body = [List [Atom "/", Atom "x", Atom "y"]], targetScopeId = 0, varargs = Nothing}),
                                 Right (Number 10.0),
                                 Right (Atom "equation"),
                                 Right (Number 18.0),
                                 Right (Lambda {args = ["x"], body = [List [Atom "*", Atom "x", Number 5.0]], targetScopeId = 0, varargs = Nothing}),
                                 Right (Lambda {args = ["x"], body = [List [Atom "*", Atom "x", Number 5.0]], targetScopeId = 0, varargs = Nothing}),
                                 Right (Bool True),
                                 Right (Bool False),
                                 Right (Number 4.0),
                                 Right (Bool False),
                                 Right (Number 1.0),
                                 Right (Bool False),
                                 Right (Bool False),
                                 Right (Atom "conditional"),
                                 Right (Atom "sup"),
                                 Right (Number 12.0),
                                 Right (Number 10.0),
                                 Right (Number 25.0),
                                 Right (Number 100.0),
                                 Right (Atom "value_a"),
                                 Right (Atom "value_b"),
                                 Right (List [Number 6.0, Number 9.0]),
                                 Right (List [Atom "sun", Atom "moon"])
                               ]
                (Left error) -> expectationFailure (show error)
            (Left error, _) -> expectationFailure (show error)
    it "correctly evaluates the second sequence of mixed language ops" $ do
      stdLib <- liftIO (readFile "./lib/stdlib.scm")
      input <- liftIO (readFile "./test/tests/berkeley/mixed-2.scm")
      let emptyCtx = getInitialScopeContext
      case parse stdLib of
        (Right stdlibExps) -> do
          case evaluateMany stdlibExps emptyCtx of
            (Right _, testCtx) -> do
              case parse input of
                (Right tree) -> do
                  let results = map fst $ evaluateManySeq tree testCtx
                  results
                    `shouldBe` [ Right (Atom "square"),
                                 Right (Number 4.0),
                                 Right (Number 10.0),
                                 Right (List [Number 1.0, Number 2.0, Number 3.0, Number 4.0]),
                                 Right (Number 1.0),
                                 Right (Number 1.0),
                                 Right (Bool True),
                                 Right (Bool False),
                                 Right (Bool True),
                                 Right (Number 1.0),
                                 Right (Number 3.0),
                                 Right (Bool False),
                                 Right (Number 3.0),
                                 Right (Atom "hello"),
                                 Right (Number 1.0),
                                 Right (Number 1.0),
                                 Right (Number 2.0),
                                 Right
                                   ( Lambda
                                       { args = [],
                                         body = [List [Atom "loop"]],
                                         targetScopeId = 0,
                                         varargs = Nothing
                                       }
                                   ),
                                 Right (Number 12.0),
                                 Right (Number 2.0),
                                 Right
                                   ( Lambda
                                       { args = ["x"],
                                         body = [List [Atom "display", Atom "x"], List [Atom "square", Atom "x"]],
                                         targetScopeId = 0,
                                         varargs = Nothing
                                       }
                                   ),
                                 Right (Number 144.0)
                               ]
                (Left error) -> expectationFailure (show error)
            (Left error, _) -> expectationFailure (show error)