module Helpers (fails, evaluateStatementOnEmptyCtx) where

import Ast (LispVal (Atom))
import Data.Either (fromRight)
import Evaluator (evaluateOnEmptyContext)
import LispError (LispError)
import Parser (parse)

fails :: Either a b -> Bool
fails (Right a) = False
fails _ = True

evaluateStatementOnEmptyCtx :: String -> Either LispError LispVal
evaluateStatementOnEmptyCtx = fst . evaluateOnEmptyContext . head . extractTrees . parse
  where
    extractTrees = fromRight [Atom ""]