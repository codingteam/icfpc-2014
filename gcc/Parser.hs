module Parser where

import Codec.Sexpr

type SSexpr = Sexpr String

parseSexpr :: String -> IO SSexpr
parseSexpr path = do
    text <- readFile path
    return $ readSexprString text