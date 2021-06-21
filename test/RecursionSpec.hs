module RecursionSpec (recursionSpec) where

import Ast (LispVal (..))
import Control.Monad.State (Monad (return), gets, runState)
import qualified Data.Map as Map (size)
import Evaluator (evaluateOn)
import Helpers (fails)
import Parser (expr)
import Test.Hspec (Spec, describe, it, shouldBe)
import qualified Text.ParserCombinators.Parsec as Parsec (ParseError, parse)
import Prelude hiding (id, lookup)

recursionSpec :: Spec
recursionSpec = do
  describe "recursion tests..." $ do
    do
      it "should enter from root" $ do
        True `shouldBe` True