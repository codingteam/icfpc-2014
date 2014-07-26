{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances, OverloadedStrings, ExistentialQuantification #-}

module GeneratorTests where

import Generator

-- | Test tuples encoding
test1 :: IO ()
test1 = testGenerator $
          load (1 :: Int ,2 :: Int ,3 :: Int)

-- | First example from specification
test2 :: IO ()
test2 = testGenerator $ do
          call1 (Mark "body") (21 :: Int)
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
    markHere "main"
    callRecursive "init" [ StackItem dir      -- var direction
                         , StackItem ("step" :: Mark)]
    i RTN

    markHere "init"
    returnS ( 0 :: Int -- init var s
            , Arg 1)

    markHere "step"
    returnS ( Arg 0
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
          getList2dItem' StackTop 1 0
          makeDumbAi StackTop

-- | Test conditionals
test8 :: IO ()
test8 = testGenerator $ do
          let x = 1 :: Number
              y = 2 :: Number
              right = 1 :: Int
              left  = 3 :: Int
          ifS (Const x `Cgt` Const y)
            (load left)
            (load right)
          makeDumbAi StackTop
          putAllFragmentsHere

test9 :: IO ()
test9 = testGenerator $ do
          let list = [ 0, 1, 2, 3 ] :: [Int]
          call (Mark "getListItem") [StackItem list, StackItem (3 :: Int)]
          i DBUG
          makeDumbAi StackTop
          getListItemDecl
          putAllFragmentsHere

