{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module EvalMonad (EvalMonad (..), EvaluationEnv (..), EvaluationState (..), unusedLambdaId) where

import Ast (LispVal (..))
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (Reader)
import Control.Monad.State
import qualified Data.Map as Map (Map, empty, fromList, lookup, map, size)
import LispError (LispError (..))

type LambdaId = Integer

data EvaluationEnv = Env
  { variables :: Map.Map String LispVal,
    isGlobal :: Bool
  }
  deriving (Show)

data EvaluationState = St
  { globalEnv :: EvaluationEnv,
    lambdaContexts :: Map.Map LambdaId EvaluationEnv
  }
  deriving (Show)

unusedLambdaId :: EvaluationState -> Integer
unusedLambdaId = fromIntegral . Map.size . lambdaContexts

type EvalMonad a = StateT EvaluationState (ExceptT LispError (Reader EvaluationEnv)) a