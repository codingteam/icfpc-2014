{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances, OverloadedStrings, ExistentialQuantification #-}

module Compiler (compileMain) where

import Control.Monad
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as M

import Syntax
import Generator
import Builtins

import Debug.Trace

data Context = Context {
        cName :: String
      , cVariables :: M.Map String Int
      } deriving (Eq)

instance Show Context where
  show c = "<" ++ cName c ++ ">"

newContext :: String -> [String] -> Context
newContext name args = Context name $ M.fromList $ zip args [0..] 

data CState = CState {
        csContexts :: [Context]
      , csFunctions :: M.Map String (Compiler ())
      , csMarkCounter :: Int
      } 

emptyCState :: CState
emptyCState = CState [] M.empty 0

newtype Compiler a = Compiler {unCompiler :: StateT CState Generator a}
  deriving (Monad, MonadState CState)

liftG :: Generator a -> Compiler a
liftG = Compiler . lift

runCompiler :: Compiler a -> Generator a
runCompiler = runCompilerState emptyCState

runCompilerState :: CState -> Compiler a -> Generator a
runCompilerState state comp = evalStateT (unCompiler comp) state

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
      Nothing -> do
                 funcs <- gets (M.keys . csFunctions)
                 if name `elem` funcs
                  then liftG $ i $ LDF $ Mark $ MkMark name
                  else fail $ "Unresolved symbol: " ++ name ++ ".\nSeen contexts: " ++ show contexts
      Just (m,n) -> liftG $ i $ LD m n
  where
    go name (ix, vars) = case M.lookup name vars of
                           Nothing -> Nothing
                           Just n -> Just (ix, n)

newName :: Compiler String
newName = do
  st <- get
  let n = csMarkCounter st + 1
  put $ st {csMarkCounter = n}
  return $ "dummy__" ++ show n

compileMain :: SyntaxNode -> Generator ()
compileMain node = runCompiler $ do
    compileMain' node
    -- Write function bodies
    compileFunctions
    -- Write standard library
    liftG stdLibrary
    -- Write IF bodies
    liftG putAllFragmentsHere
  where
    compileFunctions = do
      funcs <- gets csFunctions
      if M.null funcs
        then return ()
        else do
             forM_ (M.assocs funcs) $ \(name, code) -> do
               modify $ \st -> st {csFunctions = M.delete name (csFunctions st)}
               code
             compileFunctions


compileMain' :: SyntaxNode -> Compiler ()
compileMain' (Define "main" (args) body) = compileFunctionBody [] "main" args body
compileMain' x = error $ (show x) ++ " is not a valid main function"

compileFunctionBody :: [Context] -> String -> [String] -> [SyntaxNode] -> Compiler ()
compileFunctionBody cxts name args body = do
  currentContext <- gets csContexts
  modify $ \st -> st {csContexts = cxts ++ csContexts st}
  inContext name args $ do
      trace ("Compile " ++ name) $ return ()
      liftG $ markHere (MkMark name)
      mapM_ compileNode body
      liftG $ i $ RTN
  modify $ \st -> st {csContexts = currentContext}

rememberC :: String -> Compiler () -> Compiler ()
rememberC name code = 
  modify $ \st ->
    st {csFunctions = M.insert name code (csFunctions st)}

compileNode :: SyntaxNode -> Compiler ()
compileNode (Number x) = do
    liftG $ load x
compileNode (Define name (args) body) = do
  cxts <- gets csContexts
  rememberC name $ compileFunctionBody cxts name args body
compileNode (Identifier name) = getVariable name
compileNode (Call funcName args) = do
  forM_ args compileNode
  liftG $ if isBuiltin funcName
          then let func = fromJust $ M.lookup funcName builtins
               in do func args
          else do
               i $ LDF $ Mark $ MkMark funcName
               i $ AP $ length args
compileNode (Let inits body) = do
  name <- newName
  cxts <- gets csContexts
  rememberC name $ compileFunctionBody cxts name (getVarNames inits) body
  forM_ inits $ \(VarInit _ val) -> compileNode val
  let n = length inits
  liftG $ do
    i $ DUM n
    i $ LDF $ Mark $ MkMark name
    i $ RAP n
compileNode (If condition thenBody elseBody) = do
  ifC (compileNode condition) (forM_ thenBody compileNode) (forM_ elseBody compileNode)
compileNode x = fail $ "Unsupported node: " ++ show x

ifC :: Compiler () -> Compiler () -> Compiler () -> Compiler ()
ifC cond true false = do
  cond
  markPrefix <- newName
  let trueMark  = MkMark $ markPrefix ++ "_true"
      falseMark = MkMark $ markPrefix ++ "_false"
  liftG $ do
    i $ SEL (Mark trueMark) (Mark falseMark)
    markHere trueMark
  true
  liftG $ do
    i JOIN
    markHere falseMark
  false
  liftG $ do
    i JOIN
