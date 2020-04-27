{-# LANGUAGE LambdaCase #-}

{- | Module for converting an annotated Javalette AST to an
LLVM AST. This is expected to run after type checking and
other preprocessing/optimizations.
-}

-- TODO: Explicit export list
module LLVM.AstGen where


import Javalette.Abs
import LLVM.ADT


newtype ToADT a = ToADT a

runToADT :: ToADT a -> a
runToADT (ToADT a) = a

instance Functor ToADT where
  fmap f (ToADT a) = ToADT (f a)

instance Applicative ToADT where
  pure      = ToADT
  ff <*> aa = fmap (runToADT ff) aa

instance Monad ToADT where
  return  = ToADT
  m >>= f = f . runToADT $ m


genLlvm :: Prog -> LLVM
genLlvm = runToADT . genProg

genProg :: Prog -> ToADT LLVM
genProg = undefined
