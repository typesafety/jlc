{- | Module for defining handy helper functions to
condense construction of LLVM ADT types.
-}

module LLVM.Shorthands where

import qualified GHC.Stack as Stack

import LLVM.ADT

--
-- * Ident shorthands
--

-- | Create a global scope Ident (@var).
globalId :: String -> Ident
globalId = Ident Global

-- | Create a local scope Ident (%var).
localId :: String -> Ident
localId = Ident Local

--
-- * Type shorthands
--

-- | Turn a type @t@ into a pointer of type @t@.
toPtr :: Type -> Type
toPtr = TPointer

i32ptr :: Type
i32ptr = toPtr i32

-- | Shorthand for creating integers of size n bits.
iBit :: Int -> Type
iBit = TNBitInt

-- Shorthands for common integer types.
i32 :: Type
i32 = iBit 32

i8 :: Type
i8 = iBit 8

i1 :: Type
i1 = iBit 1

bool :: Type
bool = i1

{- | We add 1 to the length of the string to account for the and null
character. In the case that strings are changed to have other uses
than only literals as printString arguments, this would need to change.
-}
strType :: String -> Type
strType str = TPointer $ TArray (length str + 1) i8

--
-- * Array-related shorthands
--

-- | Indices to be used in getelementptr.
idx :: Int -> (Type, Source)
idx n = (i32, srcI32 n)

-- | Array of variable (0) length. "Standard".
arrType :: Type -> Type
arrType = TArray 0

{- | Representation of a JL array; represented as a 2-struct containing the
length of the array and a pointer to the actual array ("with zero length").
-}
jlArrType :: Type -> Type
jlArrType t = TStruct [i32, toPtr (arrType t)]

--
-- * Source shorthands
--

srcLitN :: Type -> Int -> Source
srcLitN t n = SVal t (LInt n)

srcD :: Double -> Source
srcD d = SVal TDouble (LDouble d)

srcI32 :: Int -> Source
srcI32 = srcLitN i32

srcTrue, srcFalse :: Source
srcTrue  = srcLitN bool 1
srcFalse = srcLitN bool 0

srcNull :: Source
srcNull = SVal TNull LNull

--
-- * Instruction shorthands (memory)
--

getelementptr :: Ident -> Type -> [(Type, Source)] -> Instruction
getelementptr id t args = IAss id (IMem $ GetElementPtr t args)

alloca :: Ident -> Type -> Instruction
alloca i t = IAss i (IMem $ Alloca t)

store :: Type -> Source -> Type -> Ident -> Instruction
store srcType src ptrType storeToId =
  INoAss $ IMem $ Store srcType src ptrType storeToId

load :: Ident -> Type -> Type -> Ident -> Instruction
load storeToId valType ptrType storeFromId =
  IAss storeToId (IMem $ Load valType ptrType storeFromId)

--
-- * Instruction shorthands (terminators)
--

ret :: Type -> Source -> Instruction
ret t s = INoAss $ ITerm $ Ret t s

vret :: Instruction
vret = INoAss $ ITerm VRet

brCond :: Source -> Label -> Label -> Instruction
brCond s l1 l2 = INoAss $ ITerm $ BrCond s l1 l2

brUncond :: Label -> Instruction
brUncond l = INoAss $ ITerm $ Br l

unreachable :: Instruction
unreachable = INoAss $ ITerm Unreachable

--
-- * Instruction shorthands (other)
--

callV :: Ident -> [Arg] -> Instruction
callV funId args = INoAss $ IOther $ Call TVoid funId args

call :: Ident -> Type -> Ident -> [Arg] -> Instruction
call assId retType funId args = IAss assId (IOther $ Call retType funId args)

bitcast :: Ident -> Type -> Source -> Type -> Instruction
bitcast assId t1 src t2 = IAss assId (IOther $ Bitcast t1 src t2)

ptrtoint :: Stack.HasCallStack
         => Ident -> Type -> Source -> Type -> Instruction
ptrtoint assId ptrType@TPointer{} src intType@TNBitInt{} =
  IAss assId (IOther $ Ptrtoint ptrType src intType)

--
-- * Instruction shorthands (arithmetic)
--

type ArithIns = Ident -> Type -> Source -> Source -> Instruction

arith :: ArithOp -> ArithIns
arith op assId retType s1 s2 = IAss assId (IArith op retType s1 s2)

add :: ArithIns
add = arith Add

fadd :: ArithIns
fadd = arith Fadd

sub :: ArithIns
sub = arith Sub

fsub :: ArithIns
fsub = arith Fsub

mul :: ArithIns
mul = arith Mul

fmul :: ArithIns
fmul = arith Fmul

sdiv :: ArithIns
sdiv = arith Sdiv

fdiv :: ArithIns
fdiv = arith Fdiv

srem :: ArithIns
srem = arith Srem

--
-- * Instruction shorthands (comparison)
--

icmp :: Ident -> ICond -> Type -> Source -> Source -> Instruction
icmp assId cond t s1 s2 = IAss assId (IOther $ Icmp cond t s1 s2)

fcmp :: Ident -> FCond -> Type -> Source -> Source -> Instruction
fcmp assId cond t s1 s2 = IAss assId (IOther $ Fcmp cond t s1 s2)

--
-- * Instruction shorthands (bitwise)
--

type BitwiseIns = Ident -> Type -> Source -> Source -> Instruction

xor :: BitwiseIns
xor assId retType s1 s2 = IAss assId (IBitwise $ Xor retType s1 s2)

and :: BitwiseIns
and assId retType s1 s2 = IAss assId (IBitwise $ And retType s1 s2)

or :: BitwiseIns
or assId retType s1 s2 = IAss assId (IBitwise $ Or retType s1 s2)
