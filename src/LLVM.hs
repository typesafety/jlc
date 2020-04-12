-- | Abstract data type for 

module LLVM where


-- | ADT for an LLVM compilation unit (module).
data LLVM = LLVM
  [TypeDef]  -- ^ Type definitions
  [VarDef]   -- ^ Global variable definitions
  [FunDef]
  -- [Extern FunDecl]
  -- [Extern (Global VarDef)]

-- * Function definitions

data FunDef = FunDef
  Type          -- ^ Return type
  Ident         -- ^ Function name
  [BasicBlock]  -- ^ Function body

data LinkageType
  = Private
  | Internal

data Attribute
  = ReadNone
  | ReadOnly
  | NounWind

data CallingConv
  = CCC
  | FastCC

data BasicBlock = BasicBlock Label [Instr]

-- * Instructions and different operations

data Instr
  = IArith Ident ArithOp Type Source Source
  | ITerm TermOp
  | IMem MemOp
  | IOther OtherOp

data ArithOp
  -- For integers
  = Add
  | Sub
  | Mul
  | Sdiv
  | Srem
  -- For doubles
  | Fadd
  | Fsub
  | Fmul
  | Fdiv

data TermOp
  = Ret
  | Br

data MemOp
  = Alloca Type
  | Load Ident Type Ident
  | Store Type Source Type Ident
  | GetElementPtr Ident Type [Type]

data OtherOp
  = Icmp ICond Type Source Source
  | Fcmp FCond Type Source Source
  | Call (Maybe Ident) Type Ident [Arg]

data FCond
  = FC_OEQ
  | FC_OGT
  | FC_OGE
  | FC_OLT
  | FC_OLE
  | FC_ONE
  | FC_ORD
  | FC_UEQ
  | FC_UGT
  | FC_UGE
  | FC_ULT
  | FC_ULE
  | FC_UNE
  | FC_UNO
  | FC_TRUE
  | FC_FALSE

data ICond
  = IC_EQ
  | IC_NE
  | IC_UGT
  | IC_UGE
  | IC_ULT
  | IC_ULE
  | IC_SGT
  | IC_SGE
  | IC_SLT
  | IC_SLE

-- * asdf

data Arg = Arg Type Ident

data Lit
  = LInt Int
  | LFloat Float
  | LNull
  | LString

data VarDef = VarDef Ident Type Source

data Source
  = SIdent Ident
  | SVal Lit

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

data Scope
  = Global
  | Local

data Ident = Ident Scope String

newtype Label = Label Ident
