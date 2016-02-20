module Eval where

import Control.Monad.Except as E
import LispData

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _)   = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func


apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (E.throwError $ NotFunction "Unrecognize primitive function" func)
                        ($ args) (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op args = liftM (Number . foldl1 op) (mapM unpackNum args)

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String s)
    | null parsed   = E.throwError $ TypeMismatch "number" $ String s
    | otherwise     = return . fst . head $ parsed
    where parsed = reads s :: [(Integer, String)]
unpackNum (List [n]) = unpackNum n
unpackNum notNum = E.throwError $ TypeMismatch "number" notNum

