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
      [ newlineSepN 2 typeDefs
      , newlineSepN 2 varDefs
      , newlineSepN 2 funDecls
      , newlineSepN 2 funDefs
      ]

instance EmitLLVM TypeDef where
  emit (TypeDef ident typ) = emit ident ++ " = type " ++ emit typ

instance EmitLLVM VarDef where
  emit (VarDef ident typ source) = mconcat
    [ emit ident, " = internal constant ", emit typ, " ", emit source ]

instance EmitLLVM FunDecl where
  emit (FunDecl typ ident pTypes) = mconcat
    [ "declare ", emit typ, " ", emit ident, "(", commaSep pTypes, ")" ]

instance EmitLLVM FunDef where
  emit (FunDef typ ident params bblocks) = mconcat
    [ "define ", emit typ, " ", emit ident, "(", commaSep params, ") {\n"
    , newlineSep bblocks
    , "}"
    ]

instance EmitLLVM BasicBlock where
  emit (BasicBlock label instrs) = mconcat
    [ indent ++ emit label, ":\n"
    , unlines $ map ((concat (replicate 2 indent) ++) . emit) instrs
    ]

instance EmitLLVM Label where
  emit (Label str) = str

instance EmitLLVM Param where
  emit (Param typ ident) = emit typ ++ " " ++ emit ident

instance EmitLLVM Source where
  emit (SIdent ident) = emit ident
  emit (SVal _ lit)   = emit lit

instance EmitLLVM Lit where
  emit = \case
    LInt n      -> show n
    LDouble d   -> show d
    LString str -> mconcat ["c\"", str, "\\00\""]
    LNull       -> "null"

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
    TStruct ts   -> "{" ++ intercalate ", " (map emit ts) ++ "}"
    t -> error $ "EmitLLVM Type: unhandled case for type `" ++ show t ++ "`"

instance EmitLLVM Instruction where
  emit (IAss ident ig) = emit ident ++ " = " ++ emit ig
  emit (INoAss ig)     = emit ig

instance EmitLLVM InstrGroup where
  emit = \case
    IArith arithOp typ s1 s2 -> mconcat
      [ emit arithOp, " ", emit typ, " ", emit s1, ", ", emit s2 ]
    ITerm termOp       -> emit termOp
    IMem memOp         -> emit memOp
    IOther otherOp     -> emit otherOp
    IBitwise bitwiseOp -> emit bitwiseOp

instance EmitLLVM ArithOp where
  emit = map toLower . show

instance EmitLLVM TermOp where
  emit = \case
    Ret typ source -> mconcat ["ret ", emit typ, " ", emit source]
    VRet -> "ret void"
    BrCond source l1 l2 -> mconcat
      [ "br i1 ", emit source, ", label %", emit l1, ", label %", emit l2 ]
    Br label -> "br label %" ++ emit label
    Unreachable -> "unreachable"

instance EmitLLVM MemOp where
  emit = \case
    Alloca typ -> "alloca " ++ emit typ
    Load valType ptrType ident -> mconcat
      [ "load ", emit valType, ", ", emit ptrType, " ", emit ident ]
    Store valType source ptrType ident -> mconcat
      [ "store "
      , emit valType, " ", emit source, ", "
      , emit ptrType, " ", emit ident
      ]
    GetElementPtr typ args -> mconcat
      [ "getelementptr ", emit typ, ", ", intercalate ", " $ map emit2 args ]
      where
        emit2 :: (Type, Source) -> String
        emit2 (t, s) = emit t ++ " " ++ emit s

instance EmitLLVM OtherOp where
  emit = \case
    Icmp iCond typ s1 s2 -> mconcat
      [ "icmp ", emit iCond, " ", emit typ, " ", emit s1, ", ", emit s2 ]
    Fcmp fCond typ s1 s2 -> mconcat
      [ "fcmp ", emit fCond, " ", emit typ, " ", emit s1, ", ", emit s2 ]
    Call retType ident args -> mconcat
      [ "call ", emit retType, " ", emit ident, "(", commaSep args, ")" ]
    Sitofp fromT src toT -> unwords
      [ "sitofp", emit fromT, emit src, "to", emit toT ]
    Ptrtoint fromT src toT -> unwords
      [ "ptrtoint", emit fromT, emit src, "to", emit toT ]
    Bitcast fromT src toT -> unwords
      [ "bitcast", emit fromT, emit src, "to", emit toT ]
    Zext fromT src toT -> unwords
      [ "zext", emit fromT, emit src, "to", emit toT ]

instance EmitLLVM BitwiseOp where
  emit = \case
    Xor typ s1 s2 -> mconcat
      [ "xor ", emit typ, " ", emit s1, ", ", emit s2 ]
    Or typ s1 s2 -> mconcat
      [ "or ", emit typ, " ", emit s1, ", ", emit s2 ]
    And typ s1 s2 -> mconcat
      [ "and ", emit typ, " ", emit s1, ", ", emit s2 ]

instance EmitLLVM ICond where
  -- Drop the "IC_" part and make the rest lowercase.
  emit = map toLower . snd . divideOn '_' . show

instance EmitLLVM FCond where
  emit = map toLower . snd . divideOn '_' . show

--
-- * Helper functions
--

newlineSep :: EmitLLVM a => [a] -> String
newlineSep = newlineSepN 1

newlineSepN :: EmitLLVM a => Int -> [a] -> String
newlineSepN n = intercalate (replicate n '\n') . map emit

commaSep :: EmitLLVM a => [a] -> String
commaSep = intercalate ", " . map emit

divideOn :: Eq a => a -> [a] -> ([a], [a])
divideOn mark xs = divide' mark ([], xs)
  where
    divide' :: Eq a => a -> ([a], [a]) -> ([a], [a])
    divide' _ t@(_, []) = t
    divide' m (l, r : rs)
      | r == m = (l, rs)
      | otherwise = divide' m (l ++ [r], rs)

indent :: String
indent = "  "
