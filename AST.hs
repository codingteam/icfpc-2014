
module AST where

data SyntaxNode = Number Int
                | Identifier String
                | Call String [SyntaxNode]
                | Define String [String] [SyntaxNode]
                | Let [VarInit] [SyntaxNode]
                | When SyntaxNode [SyntaxNode]
                | If SyntaxNode [SyntaxNode] [SyntaxNode]
     deriving Show

data VarInit = VarInit String SyntaxNode
    deriving Show

