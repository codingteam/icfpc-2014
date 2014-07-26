module Main where

import Parser (iParse, pFunc)

main :: IO ()
main = do
  let input_text = unlines [
                   "function f a b c:"
                 , "  a += b"
                 , "  c /= b"
                 , "  go a"
                 , "  halt"
                 ]
  case iParse pFunc "indented_example" input_text of
    Left  err    -> print err
    Right result -> putStrLn $ "I parsed: " ++ show result
