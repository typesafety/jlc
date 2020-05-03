{-# LANGUAGE LambdaCase #-}

{- | Module for converting an LLVM AST to a string
format that can be written to a file.
-}

-- TODO: Explicit export list
module LLVM.Emit where

import qualified Control.Monad.Writer as W

import LLVM.ADT


type Emit a = W.Writer [String] a

runEmit :: Emit a -> [String]
runEmit = W.execWriter

emit :: LLVM -> String
emit = unlines . runEmit . emitLLVM

emitLLVM :: LLVM -> Emit ()
emitLLVM (LLVM typeDefs varDefs funDefs funDecls) = do
  mapM_ emitVarDef varDefs
  mapM_ emitFunDecl funDecls
  mapM_ emitFunDef funDefs

emitVarDef :: VarDef -> Emit ()
emitVarDef = undefined

emitFunDecl :: FunDecl -> Emit ()
emitFunDecl = undefined

emitFunDef :: FunDef -> Emit ()
emitFunDef = undefined
