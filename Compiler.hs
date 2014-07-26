module Compiler(compileMain) where

import Syntax
import Generator

compileMain :: SyntaxNode -> Generator ()
compileMain (Define "main" (args) body) = compileFunctionBody "main" args body
compileMain x = error $ (show x) ++ " is not a valid main function"

compileFunctionBody :: String -> [String] -> [SyntaxNode] -> Generator ()
compileFunctionBody name argNames body = mapM_ compileNode body

compileNode :: SyntaxNode -> Generator ()
compileNode (Number x) = do
    load x