module Main where

import System.Environment
import System.IO

import Eval
import Parsers

main :: IO ()
main = getArgs >>= print . eval . readExpr . head

