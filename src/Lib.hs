module Lib
  ( Lib.parse,
    parseAndEval,
  )
where

import Ast
import Data.Char
import Evaluator (eval)
import Parser (expr)
import qualified Text.Parsec as Parsec (parse)
import Text.Parsec.Token
import Text.ParserCombinators.Parsec

parse :: String -> Either ParseError LispVal
parse = Parsec.parse expr ""

parseAndEval :: String -> String
parseAndEval xs = case parseResult of
  (Right val) -> (show . eval) val
  (Left parseError) -> show parseError
  where
    parseResult = Lib.parse xs