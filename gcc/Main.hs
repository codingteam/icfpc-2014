module Main where

import System.Environment (getArgs)
import Parser
import Syntax
import Compiler
import Generator

main :: IO ()
main = do
    args <- getArgs
    execute args

execute :: [String] -> IO ()
execute ["sexpr", path] = do
    s <- parseSexpr path
    putStrLn $ show s
execute ["parse", path] = do
    s <- parseSexpr path
    putStrLn $ show $ parse s
execute ["compile", path] = do
    s <- parseSexpr path
    testGenerator $ compileMain $ parse s
execute _ = putStrLn "sexpr path - parse sexpr\nparse path - parse syntax tree\ncompile path - compile file"
