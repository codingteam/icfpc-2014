module Builtins (builtins, isBuiltin) where

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