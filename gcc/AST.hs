
module AST where

data SyntaxNode = Number Int
                | Identifier String
                | Call String [SyntaxNode]
                | CallQ SyntaxNode [SyntaxNode]
                | Define String [String] [SyntaxNode]
                | Let [VarInit] [SyntaxNode]
                | Set String SyntaxNode
                | When SyntaxNode [SyntaxNode]
                | If SyntaxNode [SyntaxNode] [SyntaxNode]
                | DoWhile [SyntaxNode] SyntaxNode 
     deriving Show

data VarInit = VarInit String SyntaxNode
    deriving Show

