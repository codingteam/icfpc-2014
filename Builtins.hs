module Builtins(builtins) where

import Control.Monad
import qualified Data.Map as M

import Generator
import Syntax

type Builtin = [SyntaxNode] -> Generator ()

builtins :: M.Map String Builtin
builtins = M.fromList [("+", add),
                       ("-", sub),
                       ("*", multiply),
                       ("/", divide)]

chain :: Instruction -> Builtin
chain instruction = \args -> do
                      forM_ [1..(length args) - 1] (\_ -> i instruction)

add :: Builtin
add = chain ADD

sub :: Builtin
sub = chain SUB

multiply :: Builtin
multiply = chain MUL

divide :: Builtin
divide = chain DIV