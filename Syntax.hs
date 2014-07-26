module Syntax where

import Codec.Sexpr
import Data.Char(isDigit)
import Parser

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

getVarNames :: [VarInit] -> [String]
getVarNames = map (\(VarInit name _) -> name)

parse :: SSexpr -> SyntaxNode
parse sexpr | isAtom sexpr = parseAtom $ unAtom sexpr
parse sexpr | isList sexpr = parseList $ unList sexpr
parse sexpr = error $ (show sexpr) ++ " has unknown sexpr kind"

parseAtom :: String -> SyntaxNode
parseAtom atom | all isDigit atom = Number $ read atom
parseAtom atom = Identifier atom

parseList :: [SSexpr] -> SyntaxNode
parseList (define : signature : body) | isAtom define && (unAtom define) == "define" =
    Define name args $ parseExprList body
    where list = unList signature
          name = unAtom $ head list
          args = map unAtom $ tail list
parseList (let_ : initializers : body) | isAtom let_ && (unAtom let_) == "let" =
    Let inits $ parseExprList body
    where pairs [] = []
          pairs (a:b:c) = (a, b) : pairs c
          pairs x = error $ "Invalid initializers in Let: " ++ show x
          inits = map (\(n, e) -> parseVarInit n e) $ pairs (unList initializers)
parseList [when, sCondition, sBody] | isAtom when && (unAtom when) == "when" =
  When condition body
  where condition = parse sCondition
        body = parseExprList $ unList sBody
parseList [sIf, sCondition, sThen, sElse] | isAtom sIf && (unAtom sIf) == "if" =
  If condition then' else'
  where condition = parse sCondition
        then' = parseExprList $ unList sThen
        else' = parseExprList $ unList sElse
parseList (name : args) | isAtom name =
    Call (unAtom name) $ map parse args
parseList list = error $ (show list) ++ " is invalid expression"

parseExprList :: [SSexpr] -> [SyntaxNode]
parseExprList list = map parse list

parseVarInit :: SSexpr -> SSexpr -> VarInit
parseVarInit sName sValue = VarInit name value
    where name = unAtom sName
          value = parse sValue
