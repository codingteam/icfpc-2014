{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

import Control.Monad
import Control.Monad.State
import qualified Data.Map as M
import Data.Generics

data Number =
    Literal Int
  | Mark String
  deriving (Eq, Ord, Data, Typeable)

instance Show Number where
  show (Literal x) = show x
  show (Mark name) = "." ++ name

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
    MarkHere String
  | Instruction Instruction
  deriving (Eq, Show, Data, Typeable)

newtype Generator a = Generator {runGenerator :: State GenState a}
  deriving (Monad, MonadState GenState)

data GenState = GenState {
    gsPosition :: Int
  , gsResult :: [UInstruction]
  , gsFragments :: M.Map String (Generator ())
  }

resolveMarks :: [UInstruction] -> [Instruction]
resolveMarks code =
    let (_,marks) = execState (mapM go code) (0, M.empty)
    in  [everywhere (mkT $ resolve marks) instr | Instruction instr <- code]
  where
    go :: UInstruction -> State (Int, M.Map String Int) ()
    go (MarkHere name) = do
      (offset, st) <- get
      let st' = M.insert name offset st
      put (offset, st')
    go (Instruction _) = do
      modify $ \(offset, st) -> (offset+1, st)

    resolve marks (Literal n) = Literal n
    resolve marks (Mark name) =
      case M.lookup name marks of
        Nothing -> error $ "Cannot resolve mark: " ++ name
        Just offset -> Literal offset

emptyGenState :: GenState
emptyGenState = GenState 0 [] M.empty

i :: Instruction -> Generator ()
i ins = modify $ \gs ->
           gs { gsPosition = gsPosition gs + 1,
                gsResult = gsResult gs ++ [Instruction ins] }

getCurrentOffset :: Generator Int
getCurrentOffset = gets gsPosition

markHere :: String -> Generator ()
markHere name = modify $ \gs ->
                  gs {gsResult = gsResult gs ++ [MarkHere name] }

getArg :: Int -> Generator ()
getArg n = i (LD 0 n)

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

printCode :: [Instruction] -> IO ()
printCode is = forM_ is $ print

generateG :: Generator a -> Generator [UInstruction]
generateG gen = do
  st <- get
  let st' = execState (runGenerator gen) st
  return $ gsResult st'

getFragment :: String -> Generator (Generator ())
getFragment name = do
  fragments <- gets gsFragments
  case M.lookup name fragments of
    Nothing -> fail $ "Unknown fragment: " ++ name
    Just code -> return code

remember :: String -> Generator () -> Generator ()
remember name code = do
  modify $ \st ->
      st {gsFragments = M.insert name code (gsFragments st)}

putHere :: String -> Generator ()
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

test2 :: IO ()
test2 = testGenerator $ do
          call (Mark "body") (21 :: Int)
          i RTN
          markHere "body"
          getArg 0
          getArg 0
          i ADD
          i RTN
