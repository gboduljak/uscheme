{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module EvalMonad
  ( EvalMonad (..),
    EvalMonad.lookup,
    EvalMonad.extendScope,
    EvalMonad.enterScope,
    EvalMonad.deleteScope,
    EvalMonad.exitScope,
    EvalMonad.switchToScope,
    EvalMonad.currentScope,
    EvalMonad.updateInOwningScope,
  )
where

import Ast (LispVal (..))
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Reader (Reader)
import Control.Monad.State (MonadTrans (lift), State)
import Data.Map hiding (lookup)
import LispError (LispError (..))
import Scoping.Scope (Binding, Scope, ScopeId)
import Scoping.ScopeResolver (ScopeContext)
import qualified Scoping.ScopeResolver as ScopeResolver
  ( ScopeContext (ScopeContext),
    ScopeResolver,
    current,
    deleteScope,
    enter,
    exit,
    extend,
    lookup,
    switchToScope,
    updateInOwningScope,
  )
import Prelude hiding (lookup)

type EvalMonad a = ExceptT LispError (State ScopeContext) a

lookup :: String -> EvalMonad (Maybe LispVal)
lookup = lift . ScopeResolver.lookup

extendScope :: Binding -> EvalMonad Scope
extendScope = lift . ScopeResolver.extend

enterScope :: EvalMonad Scope
enterScope = lift ScopeResolver.enter

exitScope :: EvalMonad (Maybe Scope)
exitScope = lift ScopeResolver.exit

currentScope :: EvalMonad Scope
currentScope = lift ScopeResolver.current

switchToScope :: ScopeId -> EvalMonad Scope
switchToScope = lift . ScopeResolver.switchToScope

deleteScope :: ScopeId -> EvalMonad Scope
deleteScope = lift . ScopeResolver.deleteScope

updateInOwningScope :: Binding -> EvalMonad Binding
updateInOwningScope = lift . ScopeResolver.updateInOwningScope