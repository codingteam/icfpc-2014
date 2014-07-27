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
                 , "  if a >= x:"
                 , "    declare tmp"
                 , "    tmp = x"
                 , "    x = a"
                 , "    a = tmp"
                 , "  else:"
                 , "    declare n"
                 , "    a ^= x"
                 , "  if a /= b:"
                 , "    a = b"
                 , "  if x <= y:"
                 , "    x ^= y"
                 , "  else:"
                 , "    x |= y"
                 , "  halt"
                 , "  go x"
                 , "  get_lm_position 1 a b"
                 , "  get_this_ghost_idx x"
                 , "  get_ghost_starting_position x a b"
                 , "  get_ghost_current_position x a b"
                 , "  get_ghost_params x c z"
                 , "  glance_at a b"
                 , "  debug"
                 , ""
                 , "function main:"
                 , "  declare a"
                 , "  a = ref a"
                 , "  a = deref a"
                 , "  call f a a a"
                 ]
  case iParse pProgram "indented_example" input_text of
    Left  err    -> print err
    Right result -> print result
