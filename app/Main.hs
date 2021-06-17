module Main where

import Ast
import Evaluator
import Lib (parseAndEval)
import Parser
import System.IO (hFlush, stdout)
import qualified Text.ParserCombinators.Parsec as Parsec (parse)
import Text.ParserCombinators.Parsec.Error (ParseError)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalAndPrint :: String -> IO ()
evalAndPrint expr = do
  putStrLn (parseAndEval expr)

repl :: IO ()
repl = do
  x <- readPrompt "uscheme >>>"
  if x == "quit"
    then return ()
    else do
      evalAndPrint x
      repl

main :: IO ()
main = repl
