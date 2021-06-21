{-# LANGUAGE NamedFieldPuns #-}

module Ast
  ( LispVal (..),
    PrimitiveFunctionKind (..),
  )
where

data LispVal
  = DottedList [LispVal] LispVal
  | List [LispVal]
  | Vector [LispVal]
  | Atom String
  | Number Double
  | String String
  | Bool Bool
  | PrimitiveFunction {name :: String}
  | Lambda
      { args :: [String],
        body :: [LispVal],
        targetScopeId :: Int,
        varargs :: Maybe String
      }
  deriving (Eq)

data PrimitiveFunctionKind = Unary | Binary deriving (Show, Eq)

instance Show LispVal where
  show (DottedList xs x) = "(" ++ unwords (map show xs) ++ " . " ++ show x ++ ")"
  show (List xs) = "(" ++ unwords (map show xs) ++ ")"
  show (Vector xs) = "#(" ++ unwords (map show xs) ++ ")"
  show Lambda {args, body, targetScopeId} = "lambda(" ++ unwords args ++ ")"
  show PrimitiveFunction {name} = "primitive " ++ show name
  show (Atom x) = x
  show (Number x) = show x
  show (String x) = "\"" ++ x ++ "\""
  show (Bool True) = "#t"
  show (Bool False) = "#f"