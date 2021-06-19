module Main where

import Ast
import Control.Monad.State
import qualified Data.Map as Map
import EvalMonad (EvalMonad (..), EvaluationEnv (..), EvaluationState (..))
import Evaluator (performEval)
import Lib (parse, parseAndEval)
import Parser
import System.IO (hFlush, stdout)
import qualified Text.ParserCombinators.Parsec as Parsec (parse)
import Text.ParserCombinators.Parsec.Error (ParseError)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

initialEvalEnv :: EvaluationEnv
initialEvalEnv = Env {variables = Map.empty, isGlobal = True}

initialState :: EvaluationState
initialState =
  St
    { globalEnv = initialEvalEnv,
      lambdaContexts = Map.empty
    }

evalAndPrint :: String -> IO ()
evalAndPrint expr = do
  putStrLn (parseAndEval expr)

repl :: StateT EvaluationState IO ()
repl = do
  x <- liftIO $ readPrompt "uscheme >>>"
  if x == "(quit)"
    then return ()
    else do
      case parse x of
        (Right parseTree) -> do
          currentState <- get
          case performEval parseTree (globalEnv currentState) currentState of
            (Left error) -> do
              liftIO $ print error
              repl
            (Right (val, state)) -> do
              put state
              liftIO $ print val
              liftIO $ print state
              repl
        (Left error) -> do
          liftIO $ print error
          repl

main :: IO ()
main = runStateT repl initialState >>= print
