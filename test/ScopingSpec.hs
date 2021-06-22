module ScopingSpec (scopingSpec) where

import Ast (LispVal (..))
import Control.Monad.State (Monad (return), StateT (runStateT), gets, runState)
import qualified Data.Map as Map (size)
import GHC.IO
import Helpers (fails)
import Parser (expr)
import Scoping.Scope (Scope (..), ScopeId)
import Scoping.ScopeResolver (ScopeContext (..), ScopeResolver (..), current, enter, exit, extend, getInitialScopeContext, lookup)
import Test.Hspec (Spec, describe, it, shouldBe)
import qualified Text.ParserCombinators.Parsec as Parsec (ParseError, parse)
import Prelude hiding (id, lookup)

enterFromRoot :: ScopeResolver Bool
enterFromRoot = do
  enter
  scope <- current
  return (id scope == 1)

resolveClosest :: ScopeResolver [Maybe LispVal]
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

resolveDeep :: ScopeResolver (Maybe LispVal, Int)
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

resolveDeep2 :: ScopeResolver (Maybe LispVal, Int)
resolveDeep2 = do
  extend ("x", Number 1)
  enter
  enter
  enter
  enter
  enter
  x <- lookup "y"
  y <- gets (Map.size . scopes)
  return (x, y)

scopingSpec :: Spec
scopingSpec = do
  describe "lexical scoping resolution tests..." $ do
    it "should enter from root" $ do
      let (result, state) = unsafePerformIO $ runStateT enterFromRoot getInitialScopeContext
       in result `shouldBe` True
    it "should resolve closest" $ do
      let (result, state) = unsafePerformIO $ runStateT resolveClosest getInitialScopeContext
       in result `shouldBe` [Just (Number 3), Just (Number 3), Just (Number 2), Just (Number 1)]
    it "should resolve deep" $ do
      let (result, state) = unsafePerformIO $ runStateT resolveDeep getInitialScopeContext
       in result `shouldBe` (Just (Number 1), 6)
    it "should resolve deep2" $ do
      let (result, state) = unsafePerformIO $ runStateT resolveDeep2 getInitialScopeContext
       in result `shouldBe` (Nothing, 6)