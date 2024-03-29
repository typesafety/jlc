-- | (Incomplete) abstract data type for LLVM.

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

-- * Function definitions

data FunDecl = FunDecl
  Type    -- ^ Return type
  Ident   -- ^ Function Name
  [Type]  -- ^ Parameters
  deriving (Eq, Show)

data FunDef = FunDef
  Type          -- ^ Return type
  Ident         -- ^ Function name
  [Param]       -- ^ Parameters
  [BasicBlock]  -- ^ Function body
  deriving (Eq, Show)

data Ident = Ident Scope String
  deriving (Eq, Ord, Show)

data Scope
  = Global
  | Local
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
  | TVoid
  | TFloat
  | TDouble
  | TNull
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
  | IBitwise BitwiseOp
  deriving (Eq, Show)

data BitwiseOp
  = Xor Type Source Source
  | Or  Type Source Source
  | And Type Source Source
  deriving (Eq, Show)

data ArithOp
  -- For integers
  = Add
  | Sub
  | Mul
  | Sdiv
  | Srem
  -- For floating point numbers
  | Fadd
  | Fsub
  | Fmul
  | Fdiv
  deriving (Eq, Show)

data TermOp
  = Ret Type Source
  | VRet
  | BrCond Source Label Label
  | Br Label
  | Unreachable
  deriving (Eq, Show)

data MemOp
  = Alloca Type
  | Load Type Type Ident
  | Store Type Source Type Ident
  | GetElementPtr Type [(Type, Source)]
  deriving (Eq, Show)

-- These should probably be separated into different types if more
-- constructors are added.
data OtherOp
  = Icmp ICond Type Source Source
  | Fcmp FCond Type Source Source
  | Call Type Ident [Arg]
  | Sitofp Type Source Type
  | Ptrtoint Type Source Type
  | Bitcast Type Source Type
  | Zext Type Source Type
  deriving (Eq, Show)

data FCond
  = FC_OEQ
  | FC_OGT
  | FC_OGE
  | FC_OLT
  | FC_OLE
  | FC_ONE
  | FC_ORD
  | FC_TRUE
  | FC_FALSE
  deriving (Eq, Show)

data ICond
  = IC_EQ
  | IC_NE
  | IC_SGT
  | IC_SGE
  | IC_SLT
  | IC_SLE
  deriving (Eq, Show)

data Param = Param Type Ident
  deriving (Eq, Show)

data Arg = Arg Type Source
  deriving (Eq, Show)

data Lit
  = LInt Int
  | LDouble Double
  | LNull
  | LString String
  deriving (Eq, Show)

data VarDef = VarDef Ident Type Source
  deriving (Eq, Show)

data Source
  = SIdent Ident
  | SVal Type Lit
  deriving (Eq, Show)

data TypeDef = TypeDef Ident Type
  deriving (Eq, Show)

newtype Label = Label String
  deriving (Eq, Show)
