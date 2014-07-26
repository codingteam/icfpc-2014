module Syntax where

import Codec.Sexpr
import Parser

data SyntaxNode = Number Int
                | Define String [String] [SyntaxNode]
     deriving Show

parse :: SSexpr -> SyntaxNode
parse sexpr | isAtom sexpr = parseAtom $ unAtom sexpr
parse sexpr | isList sexpr = parseList $ unList sexpr
parse sexpr = error $ (show sexpr) ++ " has unknown sexpr kind"

parseAtom :: String -> SyntaxNode
parseAtom atom = Number $ read atom

parseList :: [SSexpr] -> SyntaxNode
parseList (define : signature : body) | isAtom define && (unAtom define) == "define" =
    Define name args $ parseExprList body
    where list = unList signature
          name = unAtom $ head list
          args = map unAtom $ tail list
parseList list = error $ (show list) ++ " is invalid expression"

parseExprList :: [SSexpr] -> [SyntaxNode]
parseExprList list = map parse list