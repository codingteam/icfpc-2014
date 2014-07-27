{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module AST (
  Program
, Statement(..)
, Function(..), FuncName, FuncArgs, Code
, Var(..), VarName
, Const(..)
) where

import Data.Word (Word8)

newtype Const = Const Word8 deriving Show

type VarName  = String
newtype Var   = Var VarName deriving Show

type FuncName = String
type FuncArgs = [Var]
data Function = Function FuncName FuncArgs Code deriving Show

type Code     = [Statement]

type Program  = [Function]

class Show a => Expr a
instance Expr Const
instance Expr Var

data Statement where
  Declare         ::           VarName -> Statement
  Assign          :: Expr a => VarName -> a -> Statement
  Inc             ::           VarName -> Statement
  Dec             ::           VarName -> Statement
  Add             :: Expr a => VarName -> a -> Statement
  Sub             :: Expr a => VarName -> a -> Statement
  Mul             :: Expr a => VarName -> a -> Statement
  Div             :: Expr a => VarName -> a -> Statement
  And             :: Expr a => VarName -> a -> Statement
  Or              :: Expr a => VarName -> a -> Statement
  Xor             :: Expr a => VarName -> a -> Statement
  IfLT            :: Expr a => a -> a -> Statement -> Maybe Statement -> Statement
  IfEQ            :: Expr a => a -> a -> Statement -> Maybe Statement -> Statement
  IfGT            :: Expr a => a -> a -> Statement -> Maybe Statement -> Statement
  Halt            ::           Statement
  Go              :: Expr a => a -> Statement
  GetLMPos        :: Expr a => a -> VarName -> VarName -> Statement
  GetThisGhostIdx ::           VarName -> Statement
  GetGStartPos    :: Expr a => a -> VarName -> VarName -> Statement
  GetGCurPos      :: Expr a => a -> VarName -> VarName -> Statement
  GetGParams      :: Expr a => a -> VarName -> VarName -> Statement
  GlanceAt        :: Expr a => a -> a -> VarName -> Statement
  Debug           ::           Statement
  Ref             :: Expr a => a -> Statement
  Deref           :: Expr a => a -> Statement
  Call            ::           FuncName -> FuncArgs -> Statement

deriving instance Show Statement
