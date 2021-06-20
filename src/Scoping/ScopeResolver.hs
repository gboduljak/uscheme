{-# LANGUAGE NamedFieldPuns #-}

module Scoping.ScopeResolver
  ( ScopeResolver (..),
    ScopeContext (..),
    current,
    lookup,
    enter,
    exit,
    extend,
    switchToScope,
    deleteScope,
    updateInOwningScope,
    getInitialScopeContext,
    runScopeResolver,
  )
where

import Ast (LispVal)
import Control.Monad.Identity (Identity (Identity), Monad (return))
import Control.Monad.State (MonadState (get, put), State, StateT (StateT), gets, runState)
import Data.Map (Map)
import qualified Data.Map as Map
import Scoping.Scope (Binding, Scope (..), ScopeId, parentId, rootScope)
import qualified Scoping.Scope as Scope (extend, lookup)
import Prelude hiding (id, lookup)

data ScopeContext = ScopeContext
  { scopes :: Map ScopeId Scope,
    currentScopeId :: ScopeId
  }
  deriving (Show)

type ScopeResolver a = StateT ScopeContext Identity a

runScopeResolver :: s -> State s a -> (a, s)
runScopeResolver context resolver = runState resolver context

getInitialScopeContext :: ScopeContext
getInitialScopeContext =
  ScopeContext
    { scopes = Map.fromList [(0, rootScope)],
      currentScopeId = 0
    }

createScope :: Maybe ScopeId -> ScopeResolver Scope
createScope parentId = do
  nextScopeId <- gets (Map.size . scopes)
  return
    ( Scope
        { id = nextScopeId,
          parentId = parentId,
          symbolTable = Map.empty
        }
    )

deleteScope :: ScopeId -> ScopeResolver Scope
deleteScope scopeId = do
  scopes <- gets scopes
  currentScopeId <- gets currentScopeId
  deletedScope <- getScope scopeId
  put
    ScopeContext
      { currentScopeId = currentScopeId,
        scopes = Map.delete scopeId scopes
      }
  return deletedScope

getScope :: ScopeId -> ScopeResolver Scope
getScope scopeId = do
  scopes <- gets scopes
  let currentScope = snd $ Map.elemAt scopeId scopes
   in return currentScope

current :: ScopeResolver Scope
current = do
  currentScopeId <- gets currentScopeId
  getScope currentScopeId

switchToScope :: ScopeId -> ScopeResolver Scope
switchToScope scopeId = do
  scopes <- gets scopes
  put
    ScopeContext
      { currentScopeId = scopeId,
        scopes = scopes
      }
  current

updateInOwningScope :: Binding -> ScopeResolver Binding
updateInOwningScope binding@(name, value) = do
  scopes <- gets scopes
  currentScopeId <- gets currentScopeId
  match <- lookupIn currentScopeId name
  case match of
    (Just (_, ownerScopeId)) -> do
      ownerScope <- getScope ownerScopeId
      let ownerScope' = Scope.extend ownerScope binding
      put
        ScopeContext
          { currentScopeId = currentScopeId,
            scopes = Map.insert ownerScopeId ownerScope' scopes
          }
      return binding
    Nothing -> return binding

extend :: Binding -> ScopeResolver Scope
extend binding =
  do
    scope <- current
    scopes <- gets scopes
    let scope' = Scope.extend scope binding
     in do
          put
            ScopeContext
              { currentScopeId = id scope,
                scopes = Map.insert (id scope) scope' scopes
              }
          current

lookup :: String -> ScopeResolver (Maybe LispVal)
lookup name = do
  currentScopeId <- gets currentScopeId
  match <- lookupIn currentScopeId name
  case match of
    (Just (value, scope)) -> return (Just value)
    Nothing -> return Nothing

lookupIn :: ScopeId -> String -> ScopeResolver (Maybe (LispVal, ScopeId))
lookupIn scopeId name = do
  scope <- getScope scopeId
  case Scope.lookup scope name of
    (Just entry) -> return (Just (entry, scopeId))
    _ -> case parentId scope of
      (Just parentId) -> lookupIn parentId name
      _ -> return Nothing

enter :: ScopeResolver Scope
enter = do
  scopeId <- gets currentScopeId
  scopes <- gets scopes
  newScope <- createScope (Just scopeId)
  put
    ScopeContext
      { currentScopeId = id newScope,
        scopes = Map.insert (id newScope) newScope scopes
      }
  return newScope

exit :: ScopeResolver (Maybe Scope)
exit = do
  scope <- current
  scopes <- gets scopes
  case parentId scope of
    (Just id) -> do
      put
        ScopeContext
          { currentScopeId = id,
            scopes = scopes
          }
      Just <$> current
    _ -> return Nothing