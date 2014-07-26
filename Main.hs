module Main where

import System.Environment (getArgs)
import Parser
import Syntax
import Compiler
import Generator

main :: IO ()
main = do
    args <- getArgs
    sexpr <- parseSexpr (head args)
    testGenerator $ compileMain $ parse sexpr
