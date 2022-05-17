-- Parses and prints the AST of the first command line argument
module Main where

import Lex
import ParseTree
import AST
import SemiPrint

import System.Environment

main :: IO ()
main = do
    args <- getArgs
    tokens <- tokenizeFromFileOrDie (head args)
    putStrLn $ printTU $ parseTUOrDie tokens