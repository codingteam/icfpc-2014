{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances, OverloadedStrings, ExistentialQuantification #-}

module Compiler (compileMain) where

import Control.Monad
import Control.Monad.State
import qualified Data.Map as M

import Syntax
import Generator

data Context = Context {
        cName :: String
      , cVariables :: M.Map String Int
      } deriving (Eq, Show)

emptyContext :: String -> Context
emptyContext name = Context name M.empty

data CState = CState {
        csContexts :: [Context]
      , csFunctions :: M.Map String (Compiler ())
      } 

emptyCState :: CState
emptyCState = CState [] M.empty

newtype Compiler a = Compiler {unCompiler :: StateT CState Generator a}
  deriving (Monad, MonadState CState)

liftG :: Generator a -> Compiler a
liftG = Compiler . lift

runCompiler :: Compiler a -> Generator a
runCompiler comp = evalStateT (unCompiler comp) emptyCState

inContext :: String -> Compiler a -> Compiler a
inContext name run = do
  modify $ \st -> st {csContexts = emptyContext name : csContexts st}
  result <- run
  modify $ \st -> st {csContexts = drop 1 (csContexts st)}
  return result

compileMain :: SyntaxNode -> Generator ()
compileMain node = runCompiler $ compileMain' node

compileMain' :: SyntaxNode -> Compiler ()
compileMain' (Define "main" (args) body) =
  inContext "MAIN" $
      compileFunctionBody "main" args body
compileMain' x = error $ (show x) ++ " is not a valid main function"

compileFunctionBody :: String -> [String] -> [SyntaxNode] -> Compiler ()
compileFunctionBody name argNames body =
  inContext name $
      mapM_ compileNode body

compileNode :: SyntaxNode -> Compiler ()
compileNode (Number x) = do
    liftG $ load x
compileNode (Define name (args) body) =
  modify $ \st ->
    st {csFunctions = M.insert name (compileFunctionBody name args body) (csFunctions st)}

