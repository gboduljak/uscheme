module LispError (LispError (..)) where

import Ast (LispVal (..))
import Text.Parsec (ParseError)

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | ParserError ParseError
  | Default String
  deriving (Eq)

instance Show LispError where
  show (NumArgs expected found) = "Expected " ++ show expected ++ " args: found values " ++ unwords (map show found)
  show (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ " , found " ++ show found
  show (BadSpecialForm message form) = message ++ " : " ++ show form
  show (NotFunction message func) = message ++ " : " ++ show func
  show (UnboundVar message var) = message ++ " : " ++ var
  show (ParserError error) = show error
  show (Default error) = error