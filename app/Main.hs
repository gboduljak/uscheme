module Main where

import Ast
import Control.Monad.State
import qualified Data.Map as Map
import EvalMonad (EvalMonad (..))
import Evaluator (evaluateOn)
import Lib (parse, parseAndEval)
import Scoping.ScopeResolver (ScopeContext (ScopeContext), getInitialScopeContext)
import System.IO (hFlush, stdout)
import qualified Text.ParserCombinators.Parsec as Parsec (parse)
import Text.ParserCombinators.Parsec.Error (ParseError)

main :: IO ()
main = do
  x <- runStateT repl getInitialScopeContext
  return ()

repl :: StateT ScopeContext IO ()
repl = do
  x <- liftIO $ readPrompt "uscheme >>> "
  if x == "(quit)"
    then return ()
    else do
      case parse x of
        (Right parseTree) -> do
          currentCtx <- get
          case evaluateOn parseTree currentCtx of
            (Left error, _) -> do
              liftIO $ print error
              repl
            (Right value, newCtx) -> do
              put newCtx
              liftIO $ print value
              liftIO $ print newCtx
              repl
        (Left error) -> do
          liftIO $ print error
          repl

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine
