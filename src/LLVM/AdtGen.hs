{-# LANGUAGE LambdaCase #-}

{- | Module for converting an annotated Javalette AST to an
LLVM AST. This is expected to run after type checking and
other preprocessing/optimizations.
-}

-- TODO: Explicit export list
module LLVM.AdtGen where

import qualified Control.Monad.State.Strict as ST
import qualified Control.Monad.Reader as R
import qualified Data.Map.Strict as M

import LLVM.ADT

import qualified Javalette.Abs as J


type GenADT a = R.ReaderT Signatures (ST.State Env) a

-- TODO: To be changed when I figure out what state we need.
type Env = Graph

type Signatures = M.Map Ident FunDecl

{- | Every BasicBlock in a function definition has a label, and points
to either another label (another BasicBlock), or Nothing, in which
case it returns from the function.
-}
type Graph = M.Map Label (Maybe Label)

runGenADT :: GenADT a -> a
runGenADT genAdt = ST.evalState (R.runReaderT genAdt initSigs) initEnv

initEnv :: Graph
initEnv = M.empty

initSigs :: Signatures
initSigs = M.fromList $ zip (map getId stdExtFunDefs) stdExtFunDefs
  where
    getId :: FunDecl -> Ident
    getId (FunDecl _ id _) = id

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
genLlvm = runGenADT . genProg

genProg :: J.Prog -> GenADT LLVM
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
