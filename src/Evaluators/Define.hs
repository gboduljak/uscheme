module Evaluators.Define where

import Ast (LispVal (Atom, Bool, Lambda, lambdaId))
import qualified Control.Arrow as Data.Bifunctor
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask, asks, local)
import Control.Monad.State (put)
import Control.Monad.Trans.State (gets)
import Data.Bifunctor (second)
import Data.Functor (($>))
import qualified Data.Map as Map
import EvalMonad
  ( EvalMonad,
    EvaluationEnv (..),
    EvaluationState (St, globalEnv, lambdaContexts),
  )
import Evaluator (LispError (BadSpecialForm))

define :: String -> LispVal -> EvaluationEnv -> EvalMonad EvaluationEnv
define varIdent varValue env = do
  boundEnv <- defineWithinCurrentEnv varIdent varValue
  if isGlobal env
    then do
      globalVars <- gets (variables . globalEnv)
      globalLambdaCtx <- gets lambdaContexts
      put
        ( St
            { globalEnv =
                Env
                  { variables = Map.insert varIdent varValue globalVars,
                    isGlobal = True
                  },
              lambdaContexts = globalLambdaCtx
            }
        )
      return boundEnv
    else return boundEnv

defineWithinCurrentEnv :: String -> LispVal -> EvalMonad EvaluationEnv
defineWithinCurrentEnv varIdent varValue = do
  env <- ask
  envVars <- asks variables
  lambdaEnvs <- gets lambdaContexts
  globalEnv <- gets globalEnv
  let updatedCurrentEnv = installBinding varIdent varValue env
   in let proceduresInCurrentEnv = extractProceduresFrom envVars
       in let procedureEnvsToUpdate = getEnvsToUpdate lambdaEnvs proceduresInCurrentEnv
           in let updatedProcedureEnvs = (Map.fromList . map applyEnvUpdate) procedureEnvsToUpdate
               in put
                    ( St
                        { globalEnv = globalEnv,
                          lambdaContexts = updatedProcedureEnvs <> lambdaEnvs
                        }
                    )
                    $> updatedCurrentEnv
  where
    extractProceduresFrom = Map.mapWithKey (const lambdaId) . Map.filter isLambda
    mapLambdaToItsEnv lambdaEnvs = \lambdaId -> Map.elemAt (fromIntegral lambdaId) lambdaEnvs
    applyEnvUpdate = second (installBinding varIdent varValue)
    getEnvsToUpdate lambdaEnvs envsToUpdate = map snd $ Map.toList $ Map.map (mapLambdaToItsEnv lambdaEnvs) envsToUpdate

installBinding :: String -> LispVal -> EvaluationEnv -> EvaluationEnv
installBinding varIdent varVal env =
  Env
    { variables = Map.insert varIdent varVal (variables env),
      isGlobal = isGlobal env
    }

isLambda :: LispVal -> Bool
isLambda Lambda {} = True
isLambda _ = False