{-# LANGUAGE LambdaCase #-}

{- | Module for converting an annotated Javalette AST to an
LLVM AST. This is expected to run after type checking and
other preprocessing/optimizations.
-}

-- TODO: Explicit export list
module LLVM.AstGen where

import LLVM.ADT

import qualified Javalette.Abs as J


newtype ToADT a = ToADT a

runToADT :: ToADT a -> a
runToADT (ToADT a) = a

instance Functor ToADT where
  fmap f (ToADT a) = ToADT (f a)

instance Applicative ToADT where
  pure      = ToADT
  ff <*> aa = fmap (runToADT ff) aa

instance Monad ToADT where
  return  = ToADT
  m >>= f = f . runToADT $ m

--
-- * "Boilerplate"-like content to include in .ll source files.
--

-- | Some external function definitions that should always be included.
stdExtFunDefs :: [FunDecl]
stdExtFunDefs =
  [ FunDecl i32    (globalId "readInt")     []
  , FunDecl Double (globalId "readDouble")  []
  , FunDecl Void   (globalId "printInt")    [Arg i32 (localId "n")]
  , FunDecl Void   (globalId "printDouble") [Arg Double (localId "d")]
  , FunDecl
      Void (globalId "printString") [Arg (TPointer $ iInt 8) (localId "s")]
  ]

--
-- * LLVM Generation.
--

genLlvm :: J.Prog -> LLVM
genLlvm = runToADT . genProg

genProg :: J.Prog -> ToADT LLVM
genProg (J.Program topDefs) = undefined

--
-- * Helper function for LLVM ADT constructors.
--

-- | Create a global scope Ident (@var).
globalId :: String -> Ident
globalId = Ident Global

-- | Create a local scope Ident (%var).
localId :: String -> Ident
localId = Ident Local

-- | Shorthand for creating integers of size n bits.
iInt :: Int -> Type
iInt = TNBitInt

-- | Shorthand for the very common i32 type.
i32 :: Type
i32 = iInt 32

--
-- * Various helper functions.
--

-- | Add a global string to the AST.
addGlobalStr :: Ident -> String -> LLVM -> LLVM
addGlobalStr id s llvm = llvm { llvmVarDefs = v : llvmVarDefs llvm }
  where
    v :: VarDef
    v = VarDef id (TArray (length s) (iInt 8)) (SVal $ LString s)
