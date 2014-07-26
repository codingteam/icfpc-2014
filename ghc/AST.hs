module AST (
  AST(..)
) where

import Data.Word (Word8)

type VarName = String

data Expr = Const Word8
          | Var VarName

data AST = E Expr
         | Declare VarName
         | Assign Var E
         | Inc Var
         | Dec Var
         | Add Var E
         | Sub Var E
         | Mul Var E
         | Div Var E
         | And Var E
         | Or  Var E
         | Xor Var E
         | IfLT E E AST (Maybe AST)
         | IfEQ E E AST (Maybe AST)
         | IfGTE  E AST (Maybe AST)
         | Halt
         | Go E
         | GetLMPos E Var Var
         | GetThisGhostIdx Var
         | GetGStartPos E Var Var
         | GetGCurPos E Var Var
         | GetGParams E Var Var
         | GlanceAt E E VAR
         | Debug
         | Ref E
         | Deref E
