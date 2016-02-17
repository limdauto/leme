module Eval where

import LispData

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _)   = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args


apply :: String -> [LispVal] -> LispVal
apply = undefined

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+))]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop = undefined

