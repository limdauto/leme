module LispData where

import qualified Text.ParserCombinators.Parsec as P

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser P.ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

type ThrowsError = Either LispError

instance Show LispVal where
    show (String contents) = "\"" ++ contents ++ "\""
    show (Atom name) = name
    show (Number contents) = show contents
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (List contents) = "(" ++ unwordsList contents ++ ")"
    show (DottedList x xs) = "(" ++ unwordsList x ++ " . " ++ show xs ++ ")"

instance Show LispError where
    show (UnboundVar message varname)  = message ++ ": " ++ varname
    show (BadSpecialForm message form) = message ++ ": " ++ show form
    show (NotFunction message func)    = message ++ ": " ++ show func
    show (NumArgs expected found)      = "Expected " ++ show expected
                                            ++ " args; found values " ++ unwordsList found
    show (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                            ++ ", found " ++ show found
    show (Parser parseErr)             = "Parse error at " ++ show parseErr

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show
