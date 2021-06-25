module Evaluators.Primitives.IOPrimitives (load) where

import Ast (LispVal (Atom, Bool, DottedList, IOFunction, List, Number, String))
import Control.Monad.Except
  ( MonadError (throwError),
    MonadIO (liftIO),
  )
import Data.Foldable (traverse_)
import qualified EvalMonad as EM (EvalMonad, load)
import Evaluators.Toolkits.ExpToolkit (unpackBool)
import LispError (LispError (BadSpecialForm, Default, ParserError, TypeMismatch))
import Parser (parse)
import System.Directory (doesFileExist)

load :: LispVal -> (LispVal -> EM.EvalMonad LispVal) -> EM.EvalMonad LispVal
load (List [Atom "load", path]) eval = do
  path' <- eval path
  case path' of
    (String p) -> do
      canLoad <- liftIO $ doesFileExist p
      if canLoad
        then do
          input <- EM.load p
          case parse input of
            (Right tree) -> do
              traverse_ eval tree
              return (IOFunction "load")
            (Left error) -> throwError (ParserError error)
        else throwError (Default (p ++ "does not exist."))
    _ -> throwError (TypeMismatch "string" path')
load expr _ = throwError (BadSpecialForm "ill-formed load" expr)
