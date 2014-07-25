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

-- | Dumb AI which always goes left.
test5 :: IO ()
test5 = testGenerator $ do
          callRecursive "init" [ StackItem (3 :: Int)      -- var left
                               , StackItem ("step" :: Mark)]
          i RTN

          markHere "init"
          load (42 :: Int) -- init var s
          getArg 1
          i CONS
          i RTN

          markHere "step"
          getArg 0 -- var s
          load (1 :: Int)
          i ADD
          getParentVar 0 -- var left
          i CONS
          i RTN -- return (s+1, left)

