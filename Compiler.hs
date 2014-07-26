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

newContext :: String -> [String] -> Context
newContext name args = Context name $ M.fromList $ zip args [0..] 

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

inContext :: String -> [String] -> Compiler a -> Compiler a
inContext name args run = do
  modify $ \st -> st {csContexts = newContext name args : csContexts st}
  result <- run
  modify $ \st -> st {csContexts = drop 1 (csContexts st)}
  return result

getVariable :: String -> Compiler ()
getVariable name = do
    contexts <- gets (map cVariables . csContexts)
    let x = msum $ map (go name) (zip [0..] contexts)
    case x of
      Nothing -> fail $ "Unresolved symbol: " ++ name
      Just (m,n) -> liftG $ i $ LD m n
  where
    go name (ix, vars) = case M.lookup name vars of
                           Nothing -> Nothing
                           Just n -> Just (ix, n)

compileMain :: SyntaxNode -> Generator ()
compileMain node = runCompiler $ do
    compileMain' node
    -- Write function bodies
    funcs <- gets csFunctions
    forM_ (M.keys funcs) $ \name -> do
      liftG $ markHere (MkMark name)
      let Just code = M.lookup name funcs
      code

compileMain' :: SyntaxNode -> Compiler ()
compileMain' (Define "main" (args) body) = compileFunctionBody "main" args body
compileMain' x = error $ (show x) ++ " is not a valid main function"

compileFunctionBody :: String -> [String] -> [SyntaxNode] -> Compiler ()
compileFunctionBody name args body =
  inContext name args $
      mapM_ compileNode body

compileNode :: SyntaxNode -> Compiler ()
compileNode (Number x) = do
    liftG $ load x
compileNode (Define name (args) body) =
  modify $ \st ->
    st {csFunctions = M.insert name (compileFunctionBody name args body) (csFunctions st)}
compileNode (Identifier name) = getVariable name
compileNode (Call funcName args) = do
  forM_ args compileNode
  liftG $ do
      i $ LDF $ Mark $ MkMark funcName
      i $ AP $ length args
    
compileNode x = fail $ "Unsupported node: " ++ show x

