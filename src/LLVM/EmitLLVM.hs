{-# LANGUAGE LambdaCase #-}

{- | Module for the EmitLLVM class, which defines how different types
should be printed as strings.
-}

module LLVM.EmitLLVM where

import LLVM.ADT


class EmitLLVM a where
  emit :: a -> String
