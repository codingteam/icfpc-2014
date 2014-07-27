module Parser where

import Codec.Sexpr
import System.Process

type SSexpr = Sexpr String

stripComments :: String -> String
stripComments s =
  unlines $ filter (\l -> null l || not (head l == '#')) $ lines s

parseSexpr :: String -> IO SSexpr
parseSexpr path = do
    text <- readProcess "cpp" [path] ""
    return $ readSexprString $ stripComments text
