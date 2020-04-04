-- | Abstract data type for 

module LLVM where


-- | ADT for an LLVM compilation unit (module).
data LLVM = LLVM
  [TypeDef]
  [Global VarDef]
  [FunDef]
  [Extern FunDecl]
  [Extern (Global VarDef)]

data TypeDef = TypeDef (Local Ident) Type

data VarDef
  = VarDefInstr Ident Instr
  | VarDefLit Ident Lit

newtype Global a = Global a

newtype Local a = Local a

newtype Extern a = Extern a

-- | Types in LLVM.
data Type
  = TypeNBitInt Int
  | TypeFloat
  | TypeDouble
  | TypeLabel
  | TypeVoid
  | TypeFun Type [Type]
  | TypePointer Type
  | TypeStruct [Type]
  | TypeArray Int Type

-- | Literals.
data Lit
  = LitInt Int
  | LitFloat Float
  | LitBool Bool
  | LitNull

-- | Function declarations.
data FunDecl = FunDecl Type String [Type]

-- | Function definitions.
data FunDef = FunDef
  Type          -- ^ Return type.
  String        -- ^ Function name.
  [Arg]        -- ^ Function parameters.
  [BasicBlock]  -- ^ Function body.

data BasicBlock = BasicBlock Label [Item]

data Item
  = ItemVar VarDef

-- | LLVM instructions
data Instr
  = Terminator TermInstr
  | Arithmetic ArithInstr
  | Memory
  | Other

data ArithInstr
  = Add 

-- | Terminator instructions.
data TermInstr
  = Ret
  | BrCond Type Lit Type Label Type Label
  | BrUncond Type String

newtype Label = Label String

newtype Ident = Ident String
