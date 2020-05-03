{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

{- | Module for the EmitLLVM class, which defines how different types
should be printed as strings.
-}

module LLVM.EmitLLVM where

import Data.List (intercalate)

import LLVM.ADT


-- | Defines how to print an LLVM ADT construct as a string.
class EmitLLVM a where
  emit :: a -> String

--
-- * Instances for LLVM data types.
--

instance EmitLLVM LLVM where
  emit (LLVM typeDefs varDefs funDefs funDecls) =
    intercalate "\n"
      [ emit typeDefs
      , emit varDefs
      , emit funDefs
      , emit funDecls
      ]

instance EmitLLVM TypeDef where
  emit (TypeDef id typ) = emit id ++ " = type " ++ emit typ

instance EmitLLVM VarDef where
  emit (VarDef id typ source) = mconcat
    [ emit id, " = internal constant ", emit typ, " ", emit source ]

instance EmitLLVM FunDecl where
  emit (FunDecl typ id params) = mconcat
    [ "declare ", emit typ, " ", emit id, emit params ]

instance EmitLLVM FunDef where
  emit (FunDef typ id params bblocks) = undefined -- mconcat
    -- [ "define ", emit typ, " ", emit id, ]

instance EmitLLVM Source where
  emit = undefined

instance EmitLLVM [Type] where
  emit = undefined

instance EmitLLVM Ident where
  emit (Ident scope name) = emit scope ++ name

instance EmitLLVM Scope where
  emit Global = "@"
  emit Local = "%"

instance EmitLLVM Type where
  emit = \case
    TNBitInt n   -> 'i' : show n
    TPointer typ -> emit typ ++ "*"
    TArray n typ -> mconcat ["[", show n, " x ", emit typ, "]"]
    TVoid        -> "void"
    TDouble      -> "double"
    -- Unused.
    -- TODO: Remove once confirmed that they are not (currently) used.
    -- TName Ident
    -- TLabel Label
    -- TFun Type [Type]
    -- TStruct [Type]
    -- TFloat
    -- TNull

--
-- * Instances for lists.
--

instance EmitLLVM [TypeDef] where
  emit = newlinePad

instance EmitLLVM [VarDef] where
  emit = newlinePad

instance EmitLLVM [FunDecl] where
  emit = newlinePad 

instance EmitLLVM [FunDef] where
  emit = newlinePad 

--
-- * Helper functions
--

newlinePad :: EmitLLVM a => [a] -> String
newlinePad = intercalate "\n" . map emit
