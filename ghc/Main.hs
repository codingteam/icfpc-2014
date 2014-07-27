module Main where

import Parser

main :: IO ()
main = do
  let input_text = unlines [
                   "function f a b c:"
                 , "  declare x"
                 , "  declare y"
                 , "  x = a"
                 , "  x++"
                 , " "
                 , "  y = b"
                 , "  y--"
                 , "  declare z"
                 , "  z = c"
                 , "  z += a"
                 , "  y -= b"
                 , "  z *= z"
                 , "  x /= y"
                 , "  y &= b"
                 , "  b |= a"
                 , "  x ^= x"
                 , ""
                 , "function main:"
                 , "  declare a"
                 ]
  case iParse pProgram "indented_example" input_text of
    Left  err    -> print err
    Right result -> print result
