{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances, OverloadedStrings, ExistentialQuantification #-}

module Generator where

import Control.Monad
import Control.Monad.State
import qualified Data.Map as M
import Data.String
import Data.Generics

-- | Number is literal number or pointer to named mark
-- (to be resolved at the end of generation)
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

-- | Trivial wrapper, just to handle overlapping instances for String
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

-- | Unused for now
data Cell =
    I Number
  | Cons Cell Cell
  | Closure Number
  deriving (Eq, Show)

-- | GCC instruction
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
  | TSEL Number Number
  | JOIN
  | LDF Number
  | AP Int
  | RTN
  | DUM Int
  | RAP Int
  | TRAP Int
  | STOP
  | ST Int Int
  | DBUG
  deriving (Eq, Show, Data, Typeable)

-- | Unresolved instruction
data UInstruction =
    MarkHere Mark
  | Instruction Instruction
  deriving (Eq, Show, Data, Typeable)

-- | Ariphmetic expression
data Expr =
    StackTop       -- ^ Top value on data stack. NB: be very carefull when using it in the middle of expression.
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

-- | Resolve mark names in instructions into real addresses
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

-- | Generate one instruction
i :: Instruction -> Generator ()
i ins = modify $ \gs ->
           gs { gsPosition = gsPosition gs + 1,
                gsResult = gsResult gs ++ [Instruction ins] }

getCurrentOffset :: Generator Int
getCurrentOffset = gets gsPosition

-- | Put named mark at current offset
markHere :: Mark -> Generator ()
markHere name = modify $ \gs ->
                  gs {gsResult = gsResult gs ++ [MarkHere name] }

-- | Get function argument from current frame
getArg :: Int -> Generator ()
getArg n = i (LD 0 n)

-- | Get variable from parent frame
getParentVar :: Int -> Generator ()
getParentVar n = i (LD 1 n)

-- | Things whcih can be pushed to data stack
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

-- | Rembmeber fragment of code and give it a name.
remember :: Mark -> Generator () -> Generator ()
remember name code = do
  modify $ \st ->
      st {gsFragments = M.insert name code (gsFragments st)}

-- | Get remembered fragment of code and put it at current offset
getFragment :: Mark -> Generator ()
getFragment name = do
  fragments <- gets gsFragments
  case M.lookup name fragments of
    Nothing -> fail $ "Unknown fragment: " ++ show name
    Just code -> code

-- | Get remembered fragment of code, put it at current offset with mark.
putHere :: Mark -> Generator ()
putHere fragmentName = do
  markHere fragmentName
  getFragment fragmentName

-- | Put all remembered fragments at current offset.
putAllFragmentsHere :: Generator ()
putAllFragmentsHere = do
  marks <- gets (M.keys . gsFragments)
  forM_ marks putHere

-- | Run generator.
generate :: Generator a -> [Instruction]
generate gen =
  let st = execState (runGenerator gen) emptyGenState
  in  resolveMarks $ gsResult st

testGenerator :: Generator a -> IO ()
testGenerator gen = printCode $ generate gen

-- | Call function with single argument
call1 :: ToStack x => Number -> x -> Generator ()
call1 addr x = do
  load x
  i (LDF addr)
  i (AP 1)

-- | Call function
call :: Number -> [StackItem] -> Generator ()
call addr xs = do
  forM_ xs load
  i (LDF addr)
  i $ AP $ length xs

-- | Call recursive function by using RAP.
-- Not sure when should we use this.
callRecursive :: Number -> [StackItem] -> Generator ()
callRecursive addr xs = do
  i $ DUM $ length xs
  forM_ xs load
  i (LDF addr)
  i $ RAP $ length xs

-- | Call tail-recursive function by using TRAP.
-- Not sure when should we use this.
callTailRecursive :: Number -> [StackItem] -> Generator ()
callTailRecursive addr xs = do
  i $ DUM $ length xs
  forM_ xs load
  i (LDF addr)
  i $ TRAP $ length xs

-- | Return value from function
returnS :: ToStack x => x -> Generator ()
returnS x = do
  load x
  i RTN

-- | Conditional operator.
-- Current implementation requires that if `ifS' was used,
-- then `putAllFragmentsHere' must be called near the end of program.
ifS :: Expr -> Generator () -> Generator () -> Generator ()
ifS cond true false = do
  offset <- getCurrentOffset
  load cond
  let markPrefix = show offset
      trueMark  = MkMark $ markPrefix ++ "_true"
      falseMark = MkMark $ markPrefix ++ "_false"
  i $ SEL (Mark trueMark) (Mark falseMark)
  remember trueMark $ do
      true
      i JOIN
  remember falseMark $ do
      false
      i JOIN

-- | do {...} while (..) loop.
-- Do not know yet how to implement `break' or `continue'
doWhile :: Expr -> Generator () -> Generator ()
doWhile cond body = do
  offset <- getCurrentOffset
  let markPrefix = show offset
      markStart = MkMark $ markPrefix ++ "_begin"
      markEnd   = MkMark $ markPrefix ++ "_end"
  markHere markStart
  body
  load cond
  i $ TSEL (Mark markStart) (Mark markEnd)
  markHere markEnd

getListItem :: Expr -> Int -> Generator ()
getListItem list ix = do
    load list
    go ix
  where
    go 0 = i CAR
    go n = do
      i CDR
      go (n-1)

-- | Get item from tuple
getTupleItem :: Expr  -- ^ Tuple
             -> Int   -- ^ Tuple size
             -> Int   -- ^ Item index
             -> Generator ()
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

getList2dItem' :: Expr -> Int -> Int -> Generator ()
getList2dItem' list row col = do
  getListItem list row
  getListItem StackTop col

-- | List -> Int -> Item
getListItemDecl :: Generator ()
getListItemDecl = do
  markHere "getListItem"
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


