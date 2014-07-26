{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances, OverloadedStrings, ExistentialQuantification #-}

module Builtins (builtins, isBuiltin, stdLibrary) where

import Control.Monad
import qualified Data.Map as M

import Generator
import Syntax

type Builtin = [SyntaxNode] -> Generator ()

builtins :: M.Map String Builtin
builtins = M.fromList [("+", add),
                       ("-", sub),
                       ("*", multiply),
                       ("/", divide),
                       ("=", equal),
                       (">", greater),
                       (">=", greaterOrEqual),
                       ("pair", pair),
                       ("car", car),
                       ("cdr", cdr)]

isBuiltin :: String -> Bool
isBuiltin name = M.member name builtins

unary :: Instruction -> [SyntaxNode] -> Generator ()
unary instruction [arg] = i instruction
unary instruction _     = fail $ "Invalid function call with " ++ (show instruction)

binary :: Instruction -> [SyntaxNode] -> Generator ()
binary instruction [arg1, arg2] = i instruction
binary instruction _            = fail $ "Invalid function call with " ++ (show instruction)

add :: Builtin
add = binary ADD

sub :: Builtin
sub = binary SUB

multiply :: Builtin
multiply = binary MUL

divide :: Builtin
divide = binary DIV

equal :: Builtin
equal = binary CEQ

greater :: Builtin
greater = binary CGT

greaterOrEqual :: Builtin
greaterOrEqual = binary CGTE

pair :: Builtin
pair = binary CONS

car :: Builtin
car = unary CAR

cdr :: Builtin
cdr = unary CDR


--- Library

stdLibrary :: Generator ()
stdLibrary = do
  getListItemDecl

-- | List -> Int -> Item
getListItemDecl :: Generator ()
getListItemDecl = do
  markHere "elt"
  call (Mark "getListItem_go") [StackItem $ Arg 0, StackItem $ Arg 1]
  i RTN
  markHere "getListItem_go"
  getArg 0
  ifS (Arg 1 `Ceq` Const 0)
    (i CAR)
    (do i CDR
        call (Mark "getListItem_go") [StackItem $ StackTop, StackItem $ Arg 1 `Sub` Const 1]
    )
  i RTN


