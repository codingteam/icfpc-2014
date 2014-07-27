{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module AST (
  Program
, Statement(..)
, Function(..), FuncName, FuncArgs, Code
, VarName
, Expr(..)
) where

import Data.Word (Word8)

type VarName  = String

type FuncName = String
type FuncArgs = [VarName]
data Function = Function FuncName FuncArgs Code deriving Show

type Code     = [Statement]

type Program  = [Function]

data Expr = Const Word8
          | Var VarName
  deriving Show

data Statement where
  Declare         :: VarName -> Statement
  Assign          :: VarName -> Expr -> Statement
  Inc             :: VarName -> Statement
  Dec             :: VarName -> Statement
  Add             :: VarName -> Expr -> Statement
  Sub             :: VarName -> Expr -> Statement
  Mul             :: VarName -> Expr -> Statement
  Div             :: VarName -> Expr -> Statement
  And             :: VarName -> Expr -> Statement
  Or              :: VarName -> Expr -> Statement
  Xor             :: VarName -> Expr -> Statement
  IfLT            :: Expr -> Expr -> Code -> Maybe Code -> Statement
  IfEQ            :: Expr -> Expr -> Code -> Maybe Code -> Statement
  IfGT            :: Expr -> Expr -> Code -> Maybe Code -> Statement
  Halt            :: Statement
  Go              :: Expr -> Statement
  GetLMPos        :: Expr -> VarName -> VarName -> Statement
  GetThisGhostIdx :: VarName -> Statement
  GetGStartPos    :: Expr -> VarName -> VarName -> Statement
  GetGCurPos      :: Expr -> VarName -> VarName -> Statement
  GetGParams      :: Expr -> VarName -> VarName -> Statement
  GlanceAt        :: Expr -> Expr -> VarName -> Statement
  Debug           :: Statement
  Ref             :: Expr -> Statement
  Deref           :: Expr -> Statement
  Call            :: FuncName -> FuncArgs -> Statement

deriving instance Show Statement
