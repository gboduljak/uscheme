{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module ListsOpsSpec (listsOpsSpec) where

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

listsOpsSpec :: Spec
listsOpsSpec = do
  describe "list processing language tests..." $ do
    it "correctly evaluates a sequence of mixed list operations" $ do
      stdLib <- liftIO (readFile "./lib/stdlib.scm")
      input <- liftIO (readFile "./test/tests/berkeley/list-ops.scm")
      let emptyCtx = getInitialScopeContext
      case parse stdLib of
        (Right stdlibExps) -> do
          case evaluateMany stdlibExps emptyCtx of
            (Right _, testCtx) -> do
              case parse input of
                (Right tree) -> do
                  let results = map fst $ evaluateManySeq tree testCtx
                  results
                    `shouldBe` [ Right
                                   ( Lambda
                                       { args = ["x"],
                                         body =
                                           [ List
                                               [ Atom "cond",
                                                 List [List [Atom ">", Atom "x", Number 0.0], Atom "x"],
                                                 List [List [Atom "=", Atom "x", Number 0.0], Number 0.0],
                                                 List [List [Atom "<", Atom "x", Number 0.0], List [Atom "-", Atom "x"]]
                                               ]
                                           ],
                                         targetScopeId = 0,
                                         varargs = Nothing
                                       }
                                   ),
                                 Right
                                   ( Lambda
                                       { args = ["x", "y"],
                                         body =
                                           [ List
                                               [ Atom "make-rat",
                                                 List
                                                   [ Atom "+",
                                                     List
                                                       [ Atom "*",
                                                         List [Atom "numer", Atom "x"],
                                                         List [Atom "denom", Atom "y"]
                                                       ],
                                                     List [Atom "*", List [Atom "numer", Atom "y"], List [Atom "denom", Atom "x"]]
                                                   ],
                                                 List [Atom "*", List [Atom "denom", Atom "x"], List [Atom "denom", Atom "y"]]
                                               ]
                                           ],
                                         targetScopeId = 0,
                                         varargs = Nothing
                                       }
                                   ),
                                 Right
                                   ( Lambda
                                       { args = ["x", "y"],
                                         body =
                                           [ List
                                               [ Atom "make-rat",
                                                 List
                                                   [ Atom "-",
                                                     List
                                                       [ Atom "*",
                                                         List [Atom "numer", Atom "x"],
                                                         List [Atom "denom", Atom "y"]
                                                       ],
                                                     List [Atom "*", List [Atom "numer", Atom "y"], List [Atom "denom", Atom "x"]]
                                                   ],
                                                 List [Atom "*", List [Atom "denom", Atom "x"], List [Atom "denom", Atom "y"]]
                                               ]
                                           ],
                                         targetScopeId = 0,
                                         varargs = Nothing
                                       }
                                   ),
                                 Right
                                   ( Lambda
                                       { args = ["x", "y"],
                                         body =
                                           [ List
                                               [ Atom "make-rat",
                                                 List [Atom "*", List [Atom "numer", Atom "x"], List [Atom "numer", Atom "y"]],
                                                 List [Atom "*", List [Atom "denom", Atom "x"], List [Atom "denom", Atom "y"]]
                                               ]
                                           ],
                                         targetScopeId = 0,
                                         varargs = Nothing
                                       }
                                   ),
                                 Right
                                   ( Lambda
                                       { args = ["x", "y"],
                                         body =
                                           [ List
                                               [ Atom "make-rat",
                                                 List [Atom "*", List [Atom "numer", Atom "x"], List [Atom "denom", Atom "y"]],
                                                 List [Atom "*", List [Atom "denom", Atom "x"], List [Atom "numer", Atom "y"]]
                                               ]
                                           ],
                                         targetScopeId = 0,
                                         varargs = Nothing
                                       }
                                   ),
                                 Right
                                   ( Lambda
                                       { args = ["x", "y"],
                                         body =
                                           [ List
                                               [ Atom "=",
                                                 List [Atom "*", List [Atom "numer", Atom "x"], List [Atom "denom", Atom "y"]],
                                                 List [Atom "*", List [Atom "numer", Atom "y"], List [Atom "denom", Atom "x"]]
                                               ]
                                           ],
                                         targetScopeId = 0,
                                         varargs = Nothing
                                       }
                                   ),
                                 Right (Atom "x"),
                                 Right (Number 1.0),
                                 Right (Number 2.0),
                                 Right (Atom "x"),
                                 Right (Atom "y"),
                                 Right (Atom "z"),
                                 Right (Number 1.0),
                                 Right (Number 3.0),
                                 Right (DottedList [DottedList [Number 1.0] (Number 2.0), Number 3.0] (Number 4.0)),
                                 Right
                                   ( Lambda
                                       { args = ["n", "d"],
                                         body = [List [Atom "cons", Atom "n", Atom "d"]],
                                         targetScopeId = 0,
                                         varargs = Nothing
                                       }
                                   ),
                                 Right
                                   ( Lambda
                                       { args = ["x"],
                                         body = [List [Atom "car", Atom "x"]],
                                         targetScopeId = 0,
                                         varargs = Nothing
                                       }
                                   ),
                                 Right
                                   ( Lambda
                                       { args = ["x"],
                                         body = [List [Atom "cdr", Atom "x"]],
                                         targetScopeId = 0,
                                         varargs = Nothing
                                       }
                                   ),
                                 Right
                                   ( Lambda
                                       { args = ["x"],
                                         body =
                                           [ List
                                               [ Atom "display",
                                                 List [Atom "numer", Atom "x"]
                                               ],
                                             List [Atom "display", List [Atom "quote", Atom "/"]],
                                             List [Atom "display", List [Atom "denom", Atom "x"]],
                                             List [Atom "newline"]
                                           ],
                                         targetScopeId = 0,
                                         varargs = Nothing
                                       }
                                   ),
                                 Right (Atom "one-half"),
                                 Right (IOFunction {name = "display"}),
                                 Right (Atom "one-third"),
                                 Right (IOFunction {name = "display"}),
                                 Right (IOFunction {name = "display"}),
                                 Right (IOFunction {name = "display"}),
                                 Right
                                   ( Lambda
                                       { args = ["a", "b"],
                                         body =
                                           [ List
                                               [ Atom "if",
                                                 List
                                                   [ Atom "=",
                                                     Atom "b",
                                                     Number 0.0
                                                   ],
                                                 Atom "a",
                                                 List [Atom "gcd", Atom "b", List [Atom "remainder", Atom "a", Atom "b"]]
                                               ]
                                           ],
                                         targetScopeId = 0,
                                         varargs = Nothing
                                       }
                                   ),
                                 Right
                                   ( Lambda
                                       { args = ["n", "d"],
                                         body =
                                           [ List
                                               [ Atom "let",
                                                 List
                                                   [ List [Atom "g", List [Atom "gcd", Atom "n", Atom "d"]]
                                                   ],
                                                 List
                                                   [ Atom "cons",
                                                     List [Atom "/", Atom "n", Atom "g"],
                                                     List [Atom "/", Atom "d", Atom "g"]
                                                   ]
                                               ]
                                           ],
                                         targetScopeId = 0,
                                         varargs = Nothing
                                       }
                                   ),
                                 Right (IOFunction {name = "display"}),
                                 Right (Atom "one-through-four"),
                                 Right (List [Number 1.0, Number 2.0, Number 3.0, Number 4.0]),
                                 Right (Number 1.0),
                                 Right (List [Number 2.0, Number 3.0, Number 4.0]),
                                 Right (Number 2.0),
                                 Right (List [Number 10.0, Number 1.0, Number 2.0, Number 3.0, Number 4.0]),
                                 Right (List [Number 5.0, Number 1.0, Number 2.0, Number 3.0, Number 4.0]),
                                 Right
                                   ( Lambda
                                       { args = ["proc", "items"],
                                         body =
                                           [ List
                                               [ Atom "if",
                                                 List [Atom "null?", Atom "items"],
                                                 Atom "nil",
                                                 List
                                                   [ Atom "cons",
                                                     List [Atom "proc", List [Atom "car", Atom "items"]],
                                                     List [Atom "map", Atom "proc", List [Atom "cdr", Atom "items"]]
                                                   ]
                                               ]
                                           ],
                                         targetScopeId = 0,
                                         varargs = Nothing
                                       }
                                   ),
                                 Right (List [Number 10.0, Number 2.5, Number 11.6, Number 17.0]),
                                 Right (List [Number 1.0, Number 4.0, Number 9.0, Number 16.0]),
                                 Right
                                   ( Lambda
                                       { args = ["items", "factor"],
                                         body =
                                           [ List
                                               [ Atom "map",
                                                 List [Atom "lambda", List [Atom "x"], List [Atom "*", Atom "x", Atom "factor"]],
                                                 Atom "items"
                                               ]
                                           ],
                                         targetScopeId = 0,
                                         varargs = Nothing
                                       }
                                   ),
                                 Right (List [Number 10.0, Number 20.0, Number 30.0, Number 40.0, Number 50.0]),
                                 Right
                                   ( Lambda
                                       { args = ["x"],
                                         body =
                                           [ List
                                               [ Atom "cond",
                                                 List [List [Atom "null?", Atom "x"], Number 0.0],
                                                 List [List [Atom "not", List [Atom "pair?", Atom "x"]], Number 1.0],
                                                 List
                                                   [ Atom "else",
                                                     List
                                                       [ Atom "+",
                                                         List
                                                           [ Atom "count-leaves",
                                                             List [Atom "car", Atom "x"]
                                                           ],
                                                         List [Atom "count-leaves", List [Atom "cdr", Atom "x"]]
                                                       ]
                                                   ]
                                               ]
                                           ],
                                         targetScopeId = 0,
                                         varargs = Nothing
                                       }
                                   ),
                                 Right (Atom "x"),
                                 Right (Number 4.0),
                                 Right (Number 8.0),
                                 Right
                                   ( Lambda
                                       { args = ["x"],
                                         body = [List [Atom "=", Number 1.0, List [Atom "remainder", Atom "x", Number 2.0]]],
                                         targetScopeId = 0,
                                         varargs = Nothing
                                       }
                                   ),
                                 Right
                                   ( Lambda
                                       { args = ["predicate", "sequence"],
                                         body =
                                           [ List
                                               [ Atom "cond",
                                                 List
                                                   [ List [Atom "null?", Atom "sequence"],
                                                     Atom "nil"
                                                   ],
                                                 List
                                                   [ List [Atom "predicate", List [Atom "car", Atom "sequence"]],
                                                     List
                                                       [ Atom "cons",
                                                         List [Atom "car", Atom "sequence"],
                                                         List
                                                           [ Atom "filter",
                                                             Atom "predicate",
                                                             List [Atom "cdr", Atom "sequence"]
                                                           ]
                                                       ]
                                                   ],
                                                 List [Atom "else", List [Atom "filter", Atom "predicate", List [Atom "cdr", Atom "sequence"]]]
                                               ]
                                           ],
                                         targetScopeId = 0,
                                         varargs = Nothing
                                       }
                                   ),
                                 Right (List [Number 1.0, Number 3.0, Number 5.0]),
                                 Right
                                   ( Lambda
                                       { args = ["op", "initial", "sequence"],
                                         body =
                                           [ List
                                               [ Atom "if",
                                                 List [Atom "null?", Atom "sequence"],
                                                 Atom "initial",
                                                 List
                                                   [ Atom "op",
                                                     List [Atom "car", Atom "sequence"],
                                                     List
                                                       [ Atom "accumulate",
                                                         Atom "op",
                                                         Atom "initial",
                                                         List [Atom "cdr", Atom "sequence"]
                                                       ]
                                                   ]
                                               ]
                                           ],
                                         targetScopeId = 0,
                                         varargs = Nothing
                                       }
                                   ),
                                 Right (Number 15.0),
                                 Right (Number 120.0),
                                 Right (List [Number 1.0, Number 2.0, Number 3.0, Number 4.0, Number 5.0]),
                                 Right
                                   ( Lambda
                                       { args = ["low", "high"],
                                         body =
                                           [ List
                                               [ Atom "if",
                                                 List [Atom ">", Atom "low", Atom "high"],
                                                 Atom "nil",
                                                 List [Atom "cons", Atom "low", List [Atom "enumerate-interval", List [Atom "+", Atom "low", Number 1.0], Atom "high"]]
                                               ]
                                           ],
                                         targetScopeId = 0,
                                         varargs = Nothing
                                       }
                                   ),
                                 Right (List [Number 2.0, Number 3.0, Number 4.0, Number 5.0, Number 6.0, Number 7.0]),
                                 Right
                                   ( Lambda
                                       { args = ["tree"],
                                         body =
                                           [ List
                                               [ Atom "cond",
                                                 List [List [Atom "null?", Atom "tree"], Atom "nil"],
                                                 List [List [Atom "not", List [Atom "pair?", Atom "tree"]], List [Atom "list", Atom "tree"]],
                                                 List
                                                   [ Atom "else",
                                                     List
                                                       [ Atom "append",
                                                         List [Atom "enumerate-tree", List [Atom "car", Atom "tree"]],
                                                         List [Atom "enumerate-tree", List [Atom "cdr", Atom "tree"]]
                                                       ]
                                                   ]
                                               ]
                                           ],
                                         targetScopeId = 0,
                                         varargs = Nothing
                                       }
                                   ),
                                 Right (List [Number 1.0, Number 2.0, Number 3.0, Number 4.0, Number 5.0])
                               ]
                (Left error) -> expectationFailure (show error)
            (Left error, _) -> expectationFailure (show error)