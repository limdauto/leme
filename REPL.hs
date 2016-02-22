module REPL where

import Control.Monad
import qualified Control.Monad.Except as E
import System.IO

import Eval
import Parsers

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ E.extractValue $ E.trapError (liftM show $ readExpr expr >>= eval)
