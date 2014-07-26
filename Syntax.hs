module Syntax where

data SyntaxNode = Number Int
                | Define String [String] [SyntaxNode]