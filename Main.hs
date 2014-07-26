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
execute ("compile": path: x) = do
    let output = case x of
                   [] -> "aout.gcc"
                   [f] -> f
                   _ -> error $ "Only one output file can be specified"
    s <- parseSexpr path
    let code = compileMain $ parse s
    testGenerator code
    let text = unlines $ map show $ generate code
    writeFile output text
    

execute _ = putStrLn "sexpr path - parse sexpr\nparse path - parse syntax tree\ncompile path - compile file"
