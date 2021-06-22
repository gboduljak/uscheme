module ArithmeticSpec (arithmeticSpec) where

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

arithmeticSpec :: Spec
arithmeticSpec = do
  describe "arithmetic tests..." $ do
    it "correctly evaluates SICP arithmetic examples" $ do
      input <- liftIO (readFile "./test/tests/berkeley/arithmetic.scm")
      let emptyCtx = getInitialScopeContext
      case parse input of
        (Right tree) -> do
          let testingCtx = snd $ evaluateMany tree emptyCtx
          let results = map fst $ evaluateManyParallel tree testingCtx
          results
            `shouldBe` [ Right (Number 10.0),
                         Right (Number 486.0),
                         Right (Number 666.0),
                         Right (Number 495.0),
                         Right (Number 2.0),
                         Right (Number 12.7),
                         Right (Number 75.0),
                         Right (Number 1200.0),
                         Right (Number 19.0),
                         Right (Number 57.0),
                         Right (Number 57.0)
                       ]
        (Left error) -> expectationFailure (show error)