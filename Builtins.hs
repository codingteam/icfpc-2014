module Builtins(builtins) where

import Control.Monad
import qualified Data.Map as M

import Generator
import Syntax

type Builtin = [SyntaxNode] -> Generator ()

builtins :: M.Map String Builtin
builtins = M.fromList [("+", add)]

add :: Builtin
add args = do
  forM_ [1..(length args) - 1] (\_ -> i ADD)
