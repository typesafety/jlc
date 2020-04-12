-- | Abstract data type for 

module LLVM where


-- | ADT for an LLVM compilation unit (module).
data LLVM = LLVM
  [TypeDef]
  -- [Global VarDef]
  -- [FunDef]
  -- [Extern FunDecl]
  -- [Extern (Global VarDef)]

data TypeDef = TypeDef Ident Type

data Type
  = TName Ident
  | TNBitInt Int
  | TLabel Label
  | TFun Type [Type]
  | TPointer Type
  | TStruct [Type]
  | TArray Int Type
  | Void
  | Float
  | Double
