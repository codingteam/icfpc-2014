module Parser where

import Codec.Sexpr

parseSexpr :: String -> IO (Sexpr String)
parseSexpr path = do
    text <- readFile path
    return $ readSexprString text