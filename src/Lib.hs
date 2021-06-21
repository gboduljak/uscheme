module Lib
  ( Lib.parse,
    parseAndEval,
  )
where

import Ast
import Data.Char
import Evaluator (LispError (ParserError), evaluateOnEmptyContext)
import Lexer (spaces)
import LispError (LispError)
import Parser (expr)
import qualified Text.Parsec as Parsec (parse)
import Text.ParserCombinators.Parsec (ParseError, skipMany)

parse :: String -> Either ParseError LispVal
parse = Parsec.parse (spaces >> expr) ""

parseAndEval :: String -> String
parseAndEval xs = case parseResult of
  (Right val) -> (show . evaluateOnEmptyContext) val
  (Left error) -> show error
  where
    parseResult = Lib.parse xs