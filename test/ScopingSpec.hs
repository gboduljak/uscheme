module ScopingSpec (scopingSpec) where

import Ast (LispVal (..))
import Control.Monad.State (Monad (return), gets, runState)
import qualified Data.Map as Map (size)
import Evaluator (LispError (..), performEvalEmpty)
import Helpers (fails)
import Parser (expr)
import Scoping.Resolver (Resolver (..), ResolverState (..), current, enter, exit, extend, getInitialState, lookup)
import Scoping.Scope (Scope (..), ScopeId)
import Test.Hspec (Spec, describe, it, shouldBe)
import qualified Text.ParserCombinators.Parsec as Parsec (ParseError, parse)
import Prelude hiding (id, lookup)

enterFromRoot :: Resolver Bool
enterFromRoot = do
  enter
  scope <- current
  return (id scope == 1)

resolveClosest :: Resolver [Maybe LispVal]
resolveClosest = do
  extend ("x", Number 1)
  enter
  extend ("x", Number 2)
  enter
  extend ("x", Number 3)
  enter
  resolve1 <- lookup "x"
  exit
  resolve2 <- lookup "x"
  exit
  resolve3 <- lookup "x"
  exit
  resolve4 <- lookup "x"
  return [resolve1, resolve2, resolve3, resolve4]

resolveDeep :: Resolver (Maybe LispVal, Int)
resolveDeep = do
  extend ("x", Number 1)
  enter
  enter
  enter
  enter
  enter
  x <- lookup "x"
  y <- gets (Map.size . scopes)
  return (x, y)

scopingSpec :: Spec
scopingSpec = do
  describe "scoping spec" $ do
    it "should enter from root" $ do
      let (result, state) = runState enterFromRoot getInitialState
       in result `shouldBe` True
    it "should resolve closest" $ do
      let (result, state) = runState resolveClosest getInitialState
       in result `shouldBe` [Just (Number 3), Just (Number 3), Just (Number 2), Just (Number 1)]
    it "should resolve deep" $ do
      let (result, state) = runState resolveDeep getInitialState
       in result `shouldBe` (Just (Number 1), 6)