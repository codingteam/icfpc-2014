{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module AST (
  AST(..)
, Var(..)
, FuncName
, FuncArgs
, VarName
) where

import Data.Word (Word8)

type VarName = String
type FuncName = String
type FuncArgs = [Var]

data Const = C Word8 deriving Show
data Var = V VarName deriving Show

class Show a => Expr a
instance Expr Const
instance Expr Var

data AST where
  Declare :: VarName -> AST
  Assign :: Expr a => VarName -> a -> AST
  Inc :: VarName -> AST
  Dec :: VarName -> AST
  Add :: Expr a => VarName -> a -> AST
  Sub :: Expr a => VarName -> a -> AST
  Mul :: Expr a => VarName -> a -> AST
  Div :: Expr a => VarName -> a -> AST
  And :: Expr a => VarName -> a -> AST
  Or  :: Expr a => VarName -> a -> AST
  Xor :: Expr a => VarName -> a -> AST
  IfLT :: Expr a => a -> a -> AST -> (Maybe AST) -> AST
  IfEQ :: Expr a => a -> a -> AST -> (Maybe AST) -> AST
  IfGT :: Expr a => a -> a -> AST -> (Maybe AST) -> AST
  Halt :: AST
  Go :: Expr a => a -> AST
  GetLMPos :: Expr a => a -> VarName -> VarName -> AST
  GetThisGhostIdx :: VarName -> AST
  GetGStartPos :: Expr a => a -> VarName -> VarName -> AST
  GetGCurPos :: Expr a => a -> VarName -> VarName -> AST
  GetGParams :: Expr a => a -> VarName -> VarName -> AST
  GlanceAt :: Expr a => a -> a -> VarName -> AST
  Debug :: AST
  Ref :: Expr a => a -> AST
  Deref :: Expr a => a -> AST
  Func :: FuncName -> FuncArgs -> AST -> AST
  Block :: [AST] -> AST

deriving instance Show AST
