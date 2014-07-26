module Main where

import System.Environment (getArgs)
import Parser
import Syntax

main :: IO ()
main = do
    args <- getArgs
    sexpr <- parseSexpr (head args)
    putStrLn $ show $ parse sexpr
