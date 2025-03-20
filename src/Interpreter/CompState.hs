{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}

module Interpreter.CompState where

import Control.Monad.Trans.State

data DesignState = DesignState { memorySizesLen :: !Int, memorySizes :: ![Int], subDesignSizesLen :: !Int, subDesignSizes :: ![Int] }

type DesignStateBuilder = StateT DesignState

instance MonadFail (Either String) where
  fail = Left

newMemory :: Monad m => Int -> DesignStateBuilder m Int
newMemory size = do
  modify' (\ds@DesignState{memorySizes, memorySizesLen} -> ds{memorySizes = size : memorySizes, memorySizesLen = memorySizesLen + 1})
  gets memorySizesLen

newSubDesign :: Monad m => Int -> DesignStateBuilder m Int
newSubDesign size = do
  modify' (\ds@DesignState{subDesignSizes, subDesignSizesLen} -> ds{subDesignSizes = size : subDesignSizes, subDesignSizesLen = subDesignSizesLen + 1})
  gets subDesignSizesLen

evalDesignState :: (Monad m) => DesignStateBuilder m a -> m (a, [Int], [Int])
evalDesignState st = do
  (x, DesignState _ submem _ subdes) <- runStateT st (DesignState 1 [] 1 [])
  pure (x, submem, subdes)
