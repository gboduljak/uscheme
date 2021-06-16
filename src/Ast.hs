module Ast
  ( LispVal (..),
  )
where

data LispVal
  = Atom String
  | Vector [LispVal]
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  deriving (Show, Eq)
