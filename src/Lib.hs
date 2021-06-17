module Lib
  ( Lib.parse,
    parseAndEval,
  )
where

import Ast
import Data.Char
import Evaluator (performEvalEmpty)
import Parser (expr)
import qualified Text.Parsec as Parsec (parse)
import Text.Parsec.Token
import Text.ParserCombinators.Parsec (ParseError)

parse :: String -> Either ParseError LispVal
parse = Parsec.parse expr ""

parseAndEval :: String -> String
parseAndEval xs = case parseResult of
  (Right val) -> (show . performEvalEmpty) val
  (Left error) -> show error
  where
    parseResult = Lib.parse xs