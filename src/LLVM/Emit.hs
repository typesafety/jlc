{-# LANGUAGE LambdaCase #-}

{- | Module for the EmitLLVM class, which defines how different types
should be printed as strings.
-}

module LLVM.Emit
       ( emit
       ) where

import Data.Char (toLower)
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
    intercalate "\n\n"
      [ newlineSep typeDefs
      , newlineSep varDefs
      , newlineSep funDecls
      , newlineSep funDefs
      ]

instance EmitLLVM TypeDef where
  emit (TypeDef id typ) = emit id ++ " = type " ++ emit typ

instance EmitLLVM VarDef where
  emit (VarDef id typ source) = mconcat
    [ emit id, " = internal constant ", emit typ, " ", emit source ]

instance EmitLLVM FunDecl where
  emit (FunDecl typ id pTypes) = mconcat
    [ "declare ", emit typ, " ", emit id, "(", commaSep pTypes, ")" ]

instance EmitLLVM FunDef where
  emit (FunDef typ id params bblocks) = mconcat
    [ "define ", emit typ, " ", emit id, "(", commaSep params, ") {\n"
    , newlineSep bblocks
    , "}"
    ]

instance EmitLLVM BasicBlock where
  emit (BasicBlock label instrs) = mconcat
    [ emit label, ":\n"
    , unlines $ map (("  " ++) . emit) instrs
    ]

instance EmitLLVM Label where
  emit (Label str) = str

instance EmitLLVM Param where
  emit (Param typ id) = emit typ ++ " " ++ emit id

instance EmitLLVM Source where
  emit (SIdent id) = emit id
  emit (SVal lit)  = emit lit

instance EmitLLVM Lit where
  emit = \case
    LInt n      -> show n
    LDouble d   -> show d
    LString str -> mconcat ["c", "\""]
    -- LNull  Unused?

instance EmitLLVM Ident where
  emit (Ident scope name) = emit scope ++ name

instance EmitLLVM Scope where
  emit Global = "@"
  emit Local = "%"

instance EmitLLVM Arg where
  emit (Arg typ src) = emit typ ++ " " ++ emit src

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

instance EmitLLVM Instruction where
  emit (IAss id ig) = emit id ++ " = " ++ emit ig
  emit (INoAss ig)  = emit ig

instance EmitLLVM InstrGroup where
  emit = \case
    IArith arithOp typ s1 s2 -> mconcat
      [ emit arithOp, " ", emit typ, ", ", emit s1, " ", emit s2 ]
    ITerm termOp       -> emit termOp
    IMem memOp         -> emit memOp
    IOther otherOp     -> emit otherOp
    IBitwise bitwiseOp -> emit bitwiseOp

instance EmitLLVM ArithOp where
  emit = map toLower . show

instance EmitLLVM TermOp where
  emit = \case
    Ret typ source -> mconcat ["ret ", emit typ, " ", emit source]
    VRet -> "ret"
    BrCond source l1 l2 -> mconcat
      [ "br ", emit source, " %", emit l1, " %", emit l2 ]
    Br label -> "br " ++ emit label
    Unreachable -> "unreachable"

instance EmitLLVM MemOp where
  emit = \case
    Alloca typ -> "alloca " ++ emit typ
    Load valType ptrType id -> mconcat
      [ "load ", emit valType, ", ", emit ptrType, " ", emit id ]
    Store valType source ptrType id -> mconcat
      [ "store "
      , emit valType, " ", emit source, ", "
      , emit ptrType, " ", emit id
      ]
    GetElementPtr typ args -> mconcat
      [ "getelementptr ", emit typ, ", ", intercalate ", " $ map emit2 args ]
      where
        emit2 :: (Type, Source) -> String
        emit2 (typ, src) = emit typ ++ " " ++ emit src

instance EmitLLVM OtherOp where
  emit = \case
    Icmp iCond typ s1 s2 -> mconcat
      [ "icmp ", emit iCond, " ", emit typ, " ", emit s1, ", ", emit s2 ]
    Fcmp fCond typ s1 s2 -> mconcat
      [ "fcmp ", emit fCond, " ", emit typ, " ", emit s1, ", ", emit s2 ]
    Call retType id args -> mconcat
      [ "call ", emit retType, " ", emit id, "(", commaSep args, ")" ]
    Sitofp fromT src toT -> unwords
      [ "sitofp", emit fromT, emit src, "to", emit toT ]
    Zext fromT src toT -> unwords
      [ "zext", emit fromT, emit src, "to", emit toT ]

instance EmitLLVM BitwiseOp where
  emit = \case
    Xor typ s1 s2 -> mconcat
      [ "xor ", emit typ, " ", emit s1, ", ", emit s2 ]

instance EmitLLVM ICond where
  -- Drop the "IC_" part and make the rest lowercase.
  emit = map toLower . snd . divideOn '_' . show

instance EmitLLVM FCond where
  emit = map toLower . snd . divideOn '_' . show

--
-- * Helper functions
--

newlineSep :: EmitLLVM a => [a] -> String
newlineSep = intercalate "\n" . map emit

commaSep :: EmitLLVM a => [a] -> String
commaSep = intercalate ", " . map emit

divideOn :: Eq a => a -> [a] -> ([a], [a])
divideOn mark xs = divide' mark ([], xs)
  where
    divide' :: Eq a => a -> ([a], [a]) -> ([a], [a])
    divide' _ t@(l, []) = t
    divide' mark (l, r : rs)
      | r == mark = (l, rs)
      | otherwise = divide' mark (l ++ [r], rs)
