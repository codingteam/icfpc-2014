module Main where

import System.Environment (getArgs)

import Parser

main :: IO ()
main = do
  [input] <- getArgs
  input_text <- readFile input
  case iParse pProgram input input_text of
    Left  err    -> print err
    Right result -> print result
