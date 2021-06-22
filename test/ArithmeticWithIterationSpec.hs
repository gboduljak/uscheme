module ArithmeticWithIterationSpec (arithmeticWithIterSpec) where

import Ast (LispVal (..))
import Control.Monad.State (Monad (return), MonadIO (liftIO), MonadTrans (lift), gets, runState)
import Data.Either (Either (Left, Right))
import qualified Data.Map as Map (size)
import Evaluator (evaluate, evaluateMany, evaluateManyParallel)
import Helpers (fails)
import LispError (LispError (..))
import Parser (expr, parse)
import Scoping.ScopeResolver (getInitialScopeContext)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import qualified Text.ParserCombinators.Parsec as Parsec (ParseError, parse)
import Prelude hiding (id, lookup)

arithmeticWithIterSpec :: Spec
arithmeticWithIterSpec = do
  describe "arithmetic tests with iteration..." $ do
    it "correctly evaluates SICP arithmetic with iteration" $ do
      input <- liftIO (readFile "./test/tests/berkeley/arithmetic-with-iteration.scm")
      let emptyCtx = getInitialScopeContext
      case parse input of
        (Right tree) -> do
          let testingCtx = snd $ evaluateMany tree emptyCtx
          let results = map fst $ evaluateManyParallel tree testingCtx
          results
            `shouldBe` [ Right (Atom "size"),
                         Right (Number 2.0),
                         Right (Number 10.0),
                         Right (Atom "pi"),
                         Right (Atom "radius"),
                         Right (Number 314.159),
                         Right (Atom "circumference"),
                         Right (Number 62.8318),
                         Right (Lambda {args = ["x"], body = [List [Atom "*", Atom "x", Atom "x"]], targetScopeId = 0, varargs = Nothing}),
                         Right (Number 441.0),
                         Right (Atom "square"),
                         Right (Number 441.0),
                         Right (Number 49.0),
                         Right (Number 81.0),
                         Right (Lambda {args = ["x", "y"], body = [List [Atom "+", List [Atom "square", Atom "x"], List [Atom "square", Atom "y"]]], targetScopeId = 0, varargs = Nothing}),
                         Right (Number 25.0),
                         Right (Lambda {args = ["a"], body = [List [Atom "sum-of-squares", List [Atom "+", Atom "a", Number 1.0], List [Atom "*", Atom "a", Number 2.0]]], targetScopeId = 0, varargs = Nothing}),
                         Right (Number 136.0),
                         Right (Lambda {args = ["x"], body = [List [Atom "cond", List [List [Atom ">", Atom "x", Number 0.0], Atom "x"], List [List [Atom "=", Atom "x", Number 0.0], Number 0.0], List [List [Atom "<", Atom "x", Number 0.0], List [Atom "-", Atom "x"]]]], targetScopeId = 0, varargs = Nothing}),
                         Right (Number 3.0),
                         Right (Number 0.0),
                         Right (Number 3.0),
                         Right (Lambda {args = ["a", "b"], body = [List [List [Atom "if", List [Atom ">", Atom "b", Number 0.0], Atom "+", Atom "-"], Atom "a", Atom "b"]], targetScopeId = 0, varargs = Nothing}),
                         Right (Number 5.0),
                         Right (Lambda {args = ["guess", "x"], body = [List [Atom "if", List [Atom "good-enough?", Atom "guess", Atom "x"], Atom "guess", List [Atom "sqrt-iter", List [Atom "improve", Atom "guess", Atom "x"], Atom "x"]]], targetScopeId = 0, varargs = Nothing}),
                         Right (Lambda {args = ["guess", "x"], body = [List [Atom "average", Atom "guess", List [Atom "/", Atom "x", Atom "guess"]]], targetScopeId = 0, varargs = Nothing}),
                         Right (Lambda {args = ["x", "y"], body = [List [Atom "/", List [Atom "+", Atom "x", Atom "y"], Number 2.0]], targetScopeId = 0, varargs = Nothing}),
                         Right (Lambda {args = ["guess", "x"], body = [List [Atom "<", List [Atom "abs", List [Atom "-", List [Atom "square", Atom "guess"], Atom "x"]], Number 1.0e-3]], targetScopeId = 0, varargs = Nothing}),
                         Right (Lambda {args = ["x"], body = [List [Atom "sqrt-iter", Number 1.0, Atom "x"]], targetScopeId = 0, varargs = Nothing}),
                         Right (Number 3.00009155413138),
                         Right (Number 11.704699917758145),
                         Right (Number 1.7739279023207892),
                         Right (Number 1000.000369924366),
                         Right (Lambda {args = ["x"], body = [List [Atom "define", List [Atom "good-enough?", Atom "guess"], List [Atom "<", List [Atom "abs", List [Atom "-", List [Atom "square", Atom "guess"], Atom "x"]], Number 1.0e-3]], List [Atom "define", List [Atom "improve", Atom "guess"], List [Atom "average", Atom "guess", List [Atom "/", Atom "x", Atom "guess"]]], List [Atom "define", List [Atom "sqrt-iter", Atom "guess"], List [Atom "if", List [Atom "good-enough?", Atom "guess"], Atom "guess", List [Atom "sqrt-iter", List [Atom "improve", Atom "guess"]]]], List [Atom "sqrt-iter", Number 1.0]], targetScopeId = 0, varargs = Nothing}),
                         Right (Number 3.00009155413138),
                         Right (Number 11.704699917758145),
                         Right (Number 1.7739279023207892),
                         Right (Number 1000.000369924366),
                         Right (Lambda {args = ["x"], body = [List [Atom "*", Atom "x", Atom "x", Atom "x"]], targetScopeId = 0, varargs = Nothing}),
                         Right (Lambda {args = ["term", "a", "next", "b"], body = [List [Atom "if", List [Atom ">", Atom "a", Atom "b"], Number 0.0, List [Atom "+", List [Atom "term", Atom "a"], List [Atom "sum", Atom "term", List [Atom "next", Atom "a"], Atom "next", Atom "b"]]]], targetScopeId = 0, varargs = Nothing}),
                         Right (Lambda {args = ["n"], body = [List [Atom "+", Atom "n", Number 1.0]], targetScopeId = 0, varargs = Nothing}),
                         Right (Lambda {args = ["a", "b"], body = [List [Atom "sum", Atom "cube", Atom "a", Atom "inc", Atom "b"]], targetScopeId = 0, varargs = Nothing}),
                         Right (Number 3025.0),
                         Right (Lambda {args = ["x"], body = [Atom "x"], targetScopeId = 0, varargs = Nothing}),
                         Right (Lambda {args = ["a", "b"], body = [List [Atom "sum", Atom "identity", Atom "a", Atom "inc", Atom "b"]], targetScopeId = 0, varargs = Nothing}),
                         Right (Number 55.0)
                       ]
        (Left error) -> expectationFailure (show error)