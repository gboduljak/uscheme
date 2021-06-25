module RecursionSpec (recursionSpec) where

import Ast (LispVal (..))
import Control.Monad.State (Monad (return), MonadIO (liftIO), MonadTrans (lift), gets, runState)
import Data.Either
import qualified Data.Map as Map (size)
import Evaluator (evaluate, evaluateMany, evaluateManyParallel, evaluateManySeq)
import Helpers (fails)
import LispError (LispError (..))
import Parser (expr, parse)
import Scoping.ScopeResolver (getInitialScopeContext)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import qualified Text.ParserCombinators.Parsec as Parsec (ParseError, parse)
import Prelude hiding (id, lookup)

recursionSpec :: Spec
recursionSpec = do
  describe "recursion tests..." $ do
    it "recursively evaluates fibonacci" $ do
      input <- liftIO (readFile "./test/tests/recursions/fib.scm")
      let emptyCtx = getInitialScopeContext
      case parse input of
        (Right tree) -> do
          let testingCtx = snd $ evaluateMany tree emptyCtx
          let results = map fst $ evaluateManyParallel tree testingCtx
          results
            `shouldBe` [ Right (Atom "fib"),
                         Right (Number 1),
                         Right (Number 1),
                         Right (Number 2),
                         Right (Number 3),
                         Right (Number 5),
                         Right (Number 55),
                         Right (Number 6765)
                       ]
        (Left error) -> expectationFailure (show error)
    it "recursively evaluates factorial" $ do
      input <- liftIO (readFile "./test/tests/recursions/factorial.scm")
      let emptyCtx = getInitialScopeContext
      case parse input of
        (Right tree) -> do
          let testingCtx = snd $ evaluateMany tree emptyCtx
          let results = map fst $ evaluateManyParallel tree testingCtx
          results
            `shouldBe` [ Right (Atom "factorial"),
                         Right (Number 1),
                         Right (Number 2),
                         Right (Number 6),
                         Right (Number 24),
                         Right (Number 120),
                         Right (Number 720),
                         Right (Number 5040),
                         Right (Number 40320),
                         Right (Number 362880),
                         Right (Number 3628800)
                       ]
        (Left error) -> expectationFailure (show error)
    it "recursively evaluates ackermann" $ do
      input <- liftIO (readFile "./test/tests/recursions/ackermann.scm")
      let emptyCtx = getInitialScopeContext
      case parse input of
        (Right tree) -> do
          let testingCtx = snd $ evaluateMany tree emptyCtx
          let results = map fst $ evaluateManyParallel tree testingCtx
          results
            `shouldBe` [ Right (Atom "ackermann"),
                         Right (Number 4),
                         Right (Number 61)
                       ]
        (Left error) -> expectationFailure (show error)
    it "recursively evaluates sum" $ do
      stdLib <- liftIO (readFile "./lib/stdlib.scm")
      input <- liftIO (readFile "./test/tests/recursions/sum.scm")
      let emptyCtx = getInitialScopeContext
      case parse stdLib of
        (Right stdlibExps) -> do
          case evaluateMany stdlibExps emptyCtx of
            (Right _, testCtx) -> do
              case parse input of
                (Right tree) -> do
                  let results = map fst $ evaluateManySeq tree testCtx
                  results
                    `shouldBe` [ Right
                                   ( Lambda
                                       { args = ["n", "total"],
                                         body = [List [Atom "if", List [Atom "zero?", Atom "n"], Atom "total", List [Atom "sum", List [Atom "-", Atom "n", Number 1.0], List [Atom "+", Atom "n", Atom "total"]]]],
                                         targetScopeId = 0,
                                         varargs = Nothing
                                       }
                                   ),
                                 Right (Number 501501.0)
                               ]
                (Left error) -> expectationFailure (show error)
            (Left error, _) -> expectationFailure (show error)
        (Left error) -> expectationFailure (show error)