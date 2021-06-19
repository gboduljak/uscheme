{-# LANGUAGE NamedFieldPuns #-}

module Scoping.Resolver (Resolver (..), ResolverState (..), current, lookup, enter, exit, getInitialState, extend) where

import Ast (LispVal)
import Control.Monad.State (MonadState (get, put), State, gets)
import Data.Map (Map)
import qualified Data.Map as Map
import Scoping.Scope (Binding, Scope (..), ScopeId, parentId, rootScope)
import qualified Scoping.Scope as Scope (extend, lookup)
import Prelude hiding (id, lookup)

data ResolverState = ResolverState
  { scopes :: Map ScopeId Scope,
    currentScopeId :: ScopeId
  }

type Resolver a = State ResolverState a

getInitialState :: ResolverState
getInitialState =
  ResolverState
    { scopes = Map.fromList [(0, rootScope)],
      currentScopeId = 0
    }

createScope :: Maybe ScopeId -> Resolver Scope
createScope parentId = do
  nextScopeId <- gets (Map.size . scopes)
  return
    ( Scope
        { id = nextScopeId,
          parentId = parentId,
          symbolTable = Map.empty
        }
    )

getScope :: ScopeId -> Resolver Scope
getScope scopeId = do
  scopes <- gets scopes
  let currentScope = snd $ Map.elemAt scopeId scopes
   in return currentScope

current :: Resolver Scope
current = do
  currentScopeId <- gets currentScopeId
  getScope currentScopeId

extend :: Binding -> Resolver Binding
extend binding =
  do
    scope <- current
    scopes <- gets scopes
    let scope' = Scope.extend scope binding
     in do
          put
            ResolverState
              { currentScopeId = id scope,
                scopes = Map.insert (id scope) scope' scopes
              }
          return binding

lookup :: String -> Resolver (Maybe LispVal)
lookup name = do
  currentScopeId <- gets currentScopeId
  lookupIn currentScopeId name

lookupIn :: ScopeId -> String -> Resolver (Maybe LispVal)
lookupIn scopeId name = do
  scope <- getScope scopeId
  case Scope.lookup scope name of
    (Just entry) -> return (Just entry)
    _ -> case parentId scope of
      (Just parentId) -> lookupIn parentId name
      _ -> return Nothing

enter :: Resolver Scope
enter = do
  scopeId <- gets currentScopeId
  scopes <- gets scopes
  newScope <- createScope (Just scopeId)
  put
    ResolverState
      { currentScopeId = id newScope,
        scopes = Map.insert (id newScope) newScope scopes
      }
  return newScope

exit :: Resolver (Maybe Scope)
exit = do
  scope <- current
  scopes <- gets scopes
  case parentId scope of
    (Just id) -> do
      put
        ResolverState
          { currentScopeId = id,
            scopes = scopes
          }
      Just <$> current
    _ -> return Nothing