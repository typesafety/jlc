{- | Module for defining handy helper functions to
condense construction of LLVM ADT types.
-}

module LLVM.Shorthands where

import LLVM.ADT


-- | Create a global scope Ident (@var).
globalId :: String -> Ident
globalId = Ident Global

-- | Create a local scope Ident (%var).
localId :: String -> Ident
localId = Ident Local

-- | Turn a type @t@ into a pointer of type @t@.
toPtr :: Type -> Type
toPtr = TPointer

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

srcLitN :: Type -> Int -> Source
srcLitN t n = SVal t (LInt n)

srcLitD :: Double -> Source
srcLitD d = SVal TDouble (LDouble d)

srcI32 :: Int -> Source
srcI32 = srcLitN i32

srcTrue, srcFalse :: Source
srcTrue  = srcLitN i1 1
srcFalse = srcLitN i1 0

{- | We add 1 to the length of the string to account for the and null
character. In the case that strings are changed to have other uses
than only literals as printString arguments, this would need to change.
-}
strType :: String -> Type
strType str = TPointer $ TArray (length str + 1) i8

--
-- * Instruction shorthands.
--

alloca :: Ident -> Type -> Instruction
alloca i t = IAss i (IMem $ Alloca t)

store :: Type -> Source -> Type -> Ident -> Instruction
store valType srcId ptrType storeToId =
  INoAss $ IMem $ Store valType srcId ptrType storeToId

ret :: Type -> Ident -> Instruction
ret t i = INoAss $ ITerm $ Ret t i

vret :: Instruction
vret = INoAss $ ITerm VRet

brCond :: Source -> Label -> Label -> Instruction
brCond s l1 l2 = INoAss $ ITerm $ BrCond s l1 l2

brUncond :: Label -> Instruction
brUncond l = INoAss $ ITerm $ Br l

