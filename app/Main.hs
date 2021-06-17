module Main where

import Ast
import Evaluator
import Lib (parseAndEval)
import Parser
import qualified Text.ParserCombinators.Parsec as Parsec (parse)
import Text.ParserCombinators.Parsec.Error (ParseError)

main :: IO ()
main = getLine >>= putStrLn . parseAndEval
