module Evaluator (eval) where

import Ast (LispVal (..))

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List [Atom "unquote", val]) = eval val
eval (List (Atom func : args)) = applyFunc func (map eval args)

applyFunc :: String -> [LispVal] -> LispVal
applyFunc func args = case mFuncImpl of
  (Just funcImpl) -> funcImpl args
  _ -> error "not found in primitives"
  where
    mFuncImpl = lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
  [ ("+", numericBinOp (+)),
    ("-", numericBinOp (-)),
    ("*", numericBinOp (*)),
    ("/", numericBinOp div),
    ("mod", numericBinOp mod),
    ("quotient", numericBinOp quot),
    ("remainder", numericBinOp rem),
    ("symbol?", unaryOp isSymbol),
    ("string?", unaryOp isString),
    ("number?", unaryOp isNumber),
    ("bool?", unaryOp isBool),
    ("list?", unaryOp isList)
  ]

isSymbol :: LispVal -> LispVal
isSymbol (Atom _) = Bool True
isSymbol _ = Bool False

isString :: LispVal -> LispVal
isString (Atom _) = Bool True
isString _ = Bool False

isNumber :: LispVal -> LispVal
isNumber (Number _) = Bool True
isNumber _ = Bool False

isBool :: LispVal -> LispVal
isBool (Bool _) = Bool True
isBool _ = Bool False

isList :: LispVal -> LispVal
isList (List _) = Bool True
isList (DottedList _ _) = Bool True
isList _ = Bool False

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [v] = f v
unaryOp f _ = error ""

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinOp op args = Number $ foldl1 op $ map unpackNum args
  where
    unpackNum :: LispVal -> Integer
    unpackNum (Number x) = x
    unpackNum _ = error ""