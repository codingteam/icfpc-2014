module Main where

import System.Environment (getArgs)
import Parser

main :: IO ()
main = do
    args <- getArgs
    sexpr <- parseSexpr (head args)
    putStrLn $ show sexpr
