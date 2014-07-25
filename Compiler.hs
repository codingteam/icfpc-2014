{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad
import Control.Monad.State

data Cell =
    I Int
  | Cons Cell Cell
  | Closure Int
  deriving (Eq, Show)

data Instruction =
    LDC Int
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
  | SEL Int Int
  | JOIN
  | LDF Int
  | AP Int
  | RTN
  | DUM
  | RAP Int
  | STOP
  | ST
  | DBUG
  deriving (Eq, Show)

newtype Generator a = Generator {runGenerator :: State GenState a}
  deriving (Monad, MonadState GenState)

data GenState = GenState {
    gsPosition :: Integer
  , gsResult :: [Instruction]
  } deriving (Show)

emptyGenState :: GenState
emptyGenState = GenState 0 []

i :: Instruction -> Generator ()
i ins = modify $ \gs ->
           gs { gsPosition = gsPosition gs + 1,
                gsResult = gsResult gs ++ [ins] }

class ToStack x where
  load :: x -> Generator ()

instance ToStack Int where
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

printCode :: [Instruction] -> IO ()
printCode is = forM_ is $ print

generate :: Generator a -> [Instruction]
generate gen =
  let st = execState (runGenerator gen) emptyGenState
  in  gsResult st

test1 :: IO ()
test1 = testGenerator $
          load (1 :: Int ,2 :: Int ,3 :: Int)

testGenerator :: Generator a -> IO ()
testGenerator gen = printCode $ generate gen


