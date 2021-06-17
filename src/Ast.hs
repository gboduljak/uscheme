module Ast
  ( LispVal (..),
  )
where

data LispVal
  = DottedList [LispVal] LispVal
  | List [LispVal]
  | Vector [LispVal]
  | Atom String
  | Number Integer
  | String String
  | Bool Bool
  deriving (Show, Eq)

-- instance Show LispVal where
--   show (DottedList xs x) = "(" ++ unwords (map show xs) ++ " . " ++ show x ++ ")"
--   show (List xs) = "(" ++ unwords (map show xs) ++ ")"
--   show (Vector xs) = "#(" ++ unwords (map show xs) ++ ")"
--   show (Atom x) = x
--   show (Number x) = show x
--   show (String x) = "\"" ++ x ++ "\""
--   show (Bool True) = "#t"
--   show (Bool False) = "#f"