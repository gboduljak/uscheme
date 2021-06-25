{-# LANGUAGE BlockArguments #-}

module PeterNorvigSpec (peterNorvigSpec) where

import Ast (LispVal (..))
import Control.Monad.State (Monad (return), MonadIO (liftIO), MonadTrans (lift), gets, runState)
import Data.Either (Either (Left, Right))
import qualified Data.Map as Map (size)
import Evaluator (evaluate, evaluateMany, evaluateManySeq)
import Helpers (fails)
import LispError (LispError (..))
import Parser (expr, parse)
import Scoping.ScopeResolver (getInitialScopeContext)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import qualified Text.ParserCombinators.Parsec as Parsec (ParseError, parse)
import Prelude hiding (id, lookup)

peterNorvigSpec :: Spec
peterNorvigSpec = do
  describe "mixed language features tests by Peter Norvig (https://norvig.com/lispy2.html)..." $ do
    it "correctly evaluates a sequence of mixed language ops" $ do
      stdLib <- liftIO (readFile "./lib/stdlib.scm")
      input <- liftIO (readFile "./test/tests/berkeley/peter.scm")
      let emptyCtx = getInitialScopeContext
      case parse stdLib of
        (Right stdlibExps) -> do
          case evaluateMany stdlibExps emptyCtx of
            (Right _, testCtx) -> do
              case parse input of
                (Right tree) -> do
                  let results = map fst $ evaluateManySeq tree testCtx
                  results
                    `shouldBe` [ Right (Atom "double"),
                                 Right (Number 10.0),
                                 Right (Atom "compose"),
                                 Right (List [Number 10.0]),
                                 Right (Atom "apply-twice"),
                                 Right (Number 20.0),
                                 Right (Number 80.0),
                                 Right (Atom "fact"),
                                 Right (Number 6.0),
                                 Right (Number 3.0414093201713376e64),
                                 Right
                                   ( Lambda
                                       { args = ["f"],
                                         body =
                                           [ List
                                               [ Atom "lambda",
                                                 List [Atom "x", Atom "y"],
                                                 List
                                                   [ Atom "if",
                                                     List [Atom "null?", Atom "x"],
                                                     Atom "nil",
                                                     List
                                                       [ Atom "f",
                                                         List
                                                           [ Atom "list",
                                                             List [Atom "car", Atom "x"],
                                                             List [Atom "car", Atom "y"]
                                                           ],
                                                         List
                                                           [ List [Atom "combine", Atom "f"],
                                                             List [Atom "cdr", Atom "x"],
                                                             List [Atom "cdr", Atom "y"]
                                                           ]
                                                       ]
                                                   ]
                                               ]
                                           ],
                                         targetScopeId = 0,
                                         varargs = Nothing
                                       }
                                   ),
                                 Right (Atom "zip"),
                                 Right
                                   ( List
                                       [ List [Number 1.0, Number 5.0],
                                         List [Number 2.0, Number 6.0],
                                         List [Number 3.0, Number 7.0],
                                         List [Number 4.0, Number 8.0]
                                       ]
                                   ),
                                 Right (Atom "riff-shuffle"),
                                 Right
                                   ( List
                                       [ Number 1.0,
                                         Number 5.0,
                                         Number 2.0,
                                         Number 6.0,
                                         Number 3.0,
                                         Number 7.0,
                                         Number 4.0,
                                         Number 8.0
                                       ]
                                   ),
                                 Right
                                   ( List
                                       [ Number 1.0,
                                         Number 3.0,
                                         Number 5.0,
                                         Number 7.0,
                                         Number 2.0,
                                         Number 4.0,
                                         Number 6.0,
                                         Number 8.0
                                       ]
                                   ),
                                 Right
                                   ( List
                                       [ Number 1.0,
                                         Number 2.0,
                                         Number 3.0,
                                         Number 4.0,
                                         Number 5.0,
                                         Number 6.0,
                                         Number 7.0,
                                         Number 8.0
                                       ]
                                   )
                               ]
                (Left error) -> expectationFailure (show error)
            (Left error, _) -> expectationFailure (show error)
        (Left error) -> expectationFailure (show error)