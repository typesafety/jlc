{- | Abstract data type for LLVM
-}

module LLVM.ADT where

-- | ADT for an LLVM compilation unit (module).
data LLVM = LLVM
  { llvmTypeDefs :: [TypeDef]  -- ^ Type definitions
  , llvmVarDefs  :: [VarDef]   -- ^ Global variable definitions
  , llvmFunDefs  :: [FunDef]   -- ^ Local (to the module) function definitions
  , llvmFunDecls :: [FunDecl]  -- ^ External function declarations
-- External global variables are not implemented.
  }
  deriving (Eq, Show)

instance Semigroup LLVM where
  LLVM as1 bs1 cs1 ds1 <> LLVM as2 bs2 cs2 ds2 =
    LLVM (as1 ++ as2) (bs1 ++ bs2) (cs1 ++ cs2) (ds1 ++ ds2)

instance Monoid LLVM where
  mempty = LLVM [] [] [] []

-- * Function definitions

data FunDecl = FunDecl
  Type   -- ^ Return type
  Ident  -- ^ Function Name
  [Arg]  -- ^ Parameters
  deriving (Eq, Show)

data FunDef = FunDef
  Type          -- ^ Return type
  Ident         -- ^ Function name
  [Arg]         -- ^ Parameters
  [BasicBlock]  -- ^ Function body
  deriving (Eq, Show)

data Ident = Ident Scope String
  deriving (Eq, Ord, Show)

data Type
  = TName Ident

  -- An int of Int bits, i.e. the parameter indicates the size the integer.
  | TNBitInt Int
  | TLabel Label
  | TFun Type [Type]
  | TPointer Type
  | TStruct [Type]

  -- An array of Int length, containing elements of type Type.
  | TArray Int Type
  | Void
  | Float
  | Double
  deriving (Eq, Show)

data LinkageType
  = Private
  | Internal
  deriving (Eq, Show)

data Attribute
  = ReadNone
  | ReadOnly
  | NounWind
  deriving (Eq, Show)

data CallingConv
  = CCC
  | FastCC
  deriving (Eq, Show)

data BasicBlock = BasicBlock Label [Instruction]
  deriving (Eq, Show)

data Instruction
  = IAss Ident InstrGroup
  | INoAss InstrGroup
  deriving (Eq, Show)

data InstrGroup
  = IArith ArithOp Type Source Source
  | ITerm TermOp
  | IMem MemOp
  | IOther OtherOp
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data TermOp
  = Ret
  | Br
  deriving (Eq, Show)

data MemOp
  = Alloca Type
  | Load Type Ident
  | Store Type Source Type Ident
  | GetElementPtr Type [Type]
  deriving (Eq, Show)

data OtherOp
  = Icmp ICond Type Source Source
  | Fcmp FCond Type Source Source
  | Call (Maybe Ident) Type Ident [Arg]
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data Arg = Arg Type Ident
  deriving (Eq, Show)

data Lit
  = LInt Int
  | LFloat Float
  | LNull
  | LString String
  deriving (Eq, Show)

data VarDef = VarDef Ident Type Source
  deriving (Eq, Show)

data Source
  = SIdent Ident
  | SVal Lit
  deriving (Eq, Show)

data TypeDef = TypeDef Ident Type
  deriving (Eq, Show)

data Scope
  = Global
  | Local
  deriving (Eq, Ord, Show)

newtype Label = Label Ident
  deriving (Eq, Show)