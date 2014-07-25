{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances, OverloadedStrings, ExistentialQuantification #-}

import Control.Monad
import Control.Monad.State
import qualified Data.Map as M
import Data.String
import Data.Generics

data Number =
    Literal Int
  | Mark Mark
  deriving (Eq, Ord, Data, Typeable)

instance Num Number where
  fromInteger n = Literal (fromInteger n)
  (+) = error "(+) is not implemented for Number"
  (*) = error "(*) is not implemented for Number"
  abs = error "abs is not implemented for Number"
  signum = error "signum is not implemented for Number"

newtype Mark = MkMark String
  deriving (Eq, Ord, Data, Typeable)

instance Show Mark where
  show (MkMark name) = name

instance IsString Mark where
  fromString = MkMark

instance Show Number where
  show (Literal x) = show x
  show (Mark name) = "." ++ show name

instance IsString Number where
  fromString mark = Mark (MkMark mark)

data Cell =
    I Number
  | Cons Cell Cell
  | Closure Number
  deriving (Eq, Show)

data Instruction =
    LDC Number
  | LD Int Int
  | ADD
  | SUB
  | MUL
  | DIV
  | CEQ 
  | CGT
  | CGTE
  | ATOM
  | CONS
  | CAR
  | CDR
  | SEL Number Number
  | JOIN
  | LDF Number
  | AP Int
  | RTN
  | DUM Int
  | RAP Int
  | STOP
  | ST
  | DBUG
  deriving (Eq, Show, Data, Typeable)

data UInstruction =
    MarkHere Mark
  | Instruction Instruction
  deriving (Eq, Show, Data, Typeable)

data Expr =
    StackTop
  | Const Number
  | Arg Int
  | Parent Int Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Ceq Expr Expr
  | Cgt Expr Expr
  | Cgte Expr Expr
  deriving (Eq, Data, Typeable)

instance Show Expr where
  show StackTop = "(stack)"
  show (Const n) = show n
  show (Arg n) = "(a#" ++ show n ++ ")"
  show (Parent n m) = "(parent[" ++ show n ++ "]#" ++ show m ++ ")"
  show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (Sub x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
  show (Mul x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
  show (Div x y) = "(" ++ show x ++ " / " ++ show y ++ ")"
  show (Ceq x y) = "(" ++ show x ++ " == " ++ show y ++ ")"
  show (Cgt x y) = "(" ++ show x ++ " > " ++ show y ++ ")"
  show (Cgte x y) = "(" ++ show x ++ " >= " ++ show y ++ ")"

liftBinOp :: (ToStack x, ToStack y) => Instruction -> x -> y -> Generator ()
liftBinOp instr x y = do
  load x
  load y
  i instr

instance ToStack Expr where
  load StackTop = return ()
  load (Const n) = load n
  load (Arg n) = getArg n
  load (Parent n m) = i (LD n m)
  load (Add x y) = liftBinOp ADD x y
  load (Sub x y) = liftBinOp SUB x y
  load (Mul x y) = liftBinOp MUL x y
  load (Div x y) = liftBinOp DIV x y
  load (Ceq x y) = liftBinOp CEQ x y
  load (Cgt x y) = liftBinOp CGT x y
  load (Cgte x y) = liftBinOp CGTE x y

newtype Generator a = Generator {runGenerator :: State GenState a}
  deriving (Monad, MonadState GenState)

data GenState = GenState {
    gsPosition :: Int
  , gsResult :: [UInstruction]
  , gsFragments :: M.Map Mark (Generator ())
  }

resolveMarks :: [UInstruction] -> [Instruction]
resolveMarks code =
    let (_,marks) = execState (mapM go code) (0, M.empty)
    in  [everywhere (mkT $ resolve marks) instr | Instruction instr <- code]
  where
    go :: UInstruction -> State (Int, M.Map Mark Int) ()
    go (MarkHere name) = do
      (offset, st) <- get
      let st' = M.insert name offset st
      put (offset, st')
    go (Instruction _) = do
      modify $ \(offset, st) -> (offset+1, st)

    resolve marks (Literal n) = Literal n
    resolve marks (Mark name) =
      case M.lookup name marks of
        Nothing -> error $ "Cannot resolve mark: " ++ show name
        Just offset -> Literal offset

emptyGenState :: GenState
emptyGenState = GenState 0 [] M.empty

i :: Instruction -> Generator ()
i ins = modify $ \gs ->
           gs { gsPosition = gsPosition gs + 1,
                gsResult = gsResult gs ++ [Instruction ins] }

getCurrentOffset :: Generator Int
getCurrentOffset = gets gsPosition

markHere :: Mark -> Generator ()
markHere name = modify $ \gs ->
                  gs {gsResult = gsResult gs ++ [MarkHere name] }

getArg :: Int -> Generator ()
getArg n = i (LD 0 n)

getParentVar :: Int -> Generator ()
getParentVar n = i (LD 1 n)

class ToStack x where
  load :: x -> Generator ()

instance ToStack Int where
  load x = i (LDC $ Literal x)

instance ToStack Number where
  load x = i (LDC x)

instance (ToStack a, ToStack b) => ToStack (a,b) where
  load (x,y) = do
    load x
    load y
    i CONS

instance (ToStack a, ToStack b, ToStack c) => ToStack (a,b,c) where
  load (x,y,z) = do
    load (y,z)
    load x
    i CONS

instance (ToStack a, ToStack b, ToStack c, ToStack d) => ToStack (a,b,c,d) where
  load (x,y,z,t) = do
    load (y,z,t)
    load x
    i CONS

instance ToStack Mark where
  load mark = i $ LDF (Mark mark)

instance ToStack a => ToStack [a] where
  load [] = load (0 :: Int)
  load (x:xs) = load (x, xs)

data StackItem = forall a. (ToStack a, Show a) => StackItem a

instance ToStack StackItem where
  load (StackItem x) = load x

instance Show StackItem where
  show (StackItem x) = show x

printCode :: [Instruction] -> IO ()
printCode is = forM_ is $ print

generateG :: Generator a -> Generator [UInstruction]
generateG gen = do
  st <- get
  let st' = execState (runGenerator gen) st
  return $ gsResult st'

getFragment :: Mark -> Generator (Generator ())
getFragment name = do
  fragments <- gets gsFragments
  case M.lookup name fragments of
    Nothing -> fail $ "Unknown fragment: " ++ show name
    Just code -> return code

remember :: Mark -> Generator () -> Generator ()
remember name code = do
  modify $ \st ->
      st {gsFragments = M.insert name code (gsFragments st)}

putHere :: Mark -> Generator ()
putHere fragmentName = do
  markHere fragmentName
  code <- getFragment fragmentName
  code

generate :: Generator a -> [Instruction]
generate gen =
  let st = execState (runGenerator gen) emptyGenState
  in  resolveMarks $ gsResult st

test1 :: IO ()
test1 = testGenerator $
          load (1 :: Int ,2 :: Int ,3 :: Int)

testGenerator :: Generator a -> IO ()
testGenerator gen = printCode $ generate gen

call :: ToStack x => Number -> x -> Generator ()
call addr x = do
  load x
  i (LDF addr)
  i (AP 1)

returnS :: ToStack x => x -> Generator ()
returnS x = do
  load x
  i RTN

callRecursive :: Number -> [StackItem] -> Generator ()
callRecursive addr xs = do
  i $ DUM $ length xs
  forM_ xs load
  i (LDF addr)
  i $ RAP $ length xs

getListItem :: Expr -> Int -> Generator ()
getListItem list ix = do
    load list
    go ix
  where
    go 0 = i CAR
    go n = do
      i CDR
      go (n-1)

getTupleItem :: Expr -> Int -> Int -> Generator ()
getTupleItem tuple size ix = do
    load tuple
    go ix
  where
    go 0
      | ix == size-1 = return ()
      | otherwise    = i CAR
    go n = do
      i CDR
      go (n-1)

getList2dItem :: Expr -> Int -> Int -> Generator ()
getList2dItem list row col = do
  getListItem list row
  getListItem StackTop col

-- | First example from specification
test2 :: IO ()
test2 = testGenerator $ do
          call (Mark "body") (21 :: Int)
          i RTN
          markHere "body"
          getArg 0
          getArg 0
          i ADD
          i RTN

-- | Default program in JS GCC interpreter
test3 :: IO ()
test3 = testGenerator $ do
          returnS (0 :: Int, "step" :: Mark)
          markHere "step"
          returnS (0 :: Int, 1 :: Int) 

-- | Test lists encoding
test4 :: IO ()
test4 = testGenerator $ do
          load ([1,2,3] :: [Int])

makeDumbAi :: Expr -> Generator ()
makeDumbAi dir = do
    callRecursive "init" [ StackItem dir      -- var direction
                         , StackItem ("step" :: Mark)]
    i RTN

    markHere "init"
    returnS ( 42 :: Int -- init var s
            , Arg 1)

    markHere "step"
    returnS ( Const 1 `Add` Arg 0 -- s + 1
            , Parent 1 0 -- var direction
            )

-- | Dumb AI which always goes left.
test5 :: IO ()
test5 = testGenerator $ makeDumbAi (Const 3)

-- | Test expressions generation
test6 :: IO ()
test6 = testGenerator $ do
          returnS $ Arg 1 `Mul` (Const 1 `Add` Const 3)

-- | Dooes the same as test5, but also checks loading/handling 2D lists
test7 :: IO ()
test7 = testGenerator $ do
          let grid = [ [0, 1, 2]
                     , [3, 4, 5]
                     ] :: [[Int]]
          load grid
          getList2dItem StackTop 1 0
          makeDumbAi StackTop

