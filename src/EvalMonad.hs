{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module EvalMonad (EvalMonad (..), VariablesEnv (..)) where

import Ast (LispVal (..))
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (Reader)
import qualified Data.Map as Map (Map, empty, fromList, lookup, map)
import LispError (LispError (..))

type VariablesEnv = Map.Map String LispVal

type EvalMonad a = ExceptT LispError (Reader VariablesEnv) a
