{-# LANGUAGE NamedFieldPuns #-}

module Scoping.Scope (Scope (..), ScopeId, Binding, extend, lookup, rootScope) where

import Ast (LispVal)
import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (id, lookup)

type Binding = (String, LispVal)

type ScopeId = Int

rootScope :: Scope
rootScope =
  Scope
    { id = 0,
      parentId = Nothing,
      symbolTable = Map.empty
    }

data Scope = Scope
  { id :: ScopeId,
    parentId :: Maybe ScopeId,
    symbolTable :: Map String LispVal
  }
  deriving (Show)

extend :: Scope -> Binding -> Scope
extend scope (name, value) =
  Scope
    { id = id scope,
      parentId = parentId scope,
      symbolTable = Map.insert name value (symbolTable scope)
    }

lookup :: Scope -> String -> Maybe LispVal
lookup Scope {symbolTable} name = Map.lookup name symbolTable