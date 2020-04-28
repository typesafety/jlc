{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Module for converting an annotated Javalette AST to an
LLVM AST. This is expected to run after type checking and
other preprocessing/optimizations.
-}

-- TODO: Explicit export list
module LLVM.AdtGen where

import Lens.Micro.Platform

import qualified Control.Monad.Reader as R
import qualified Control.Monad.State.Strict as ST
import qualified Control.Monad.Writer.Strict as W
import qualified Data.Map.Strict as M

import LLVM.ADT

import qualified Javalette.Abs as J


type Graph = M.Map Label (Maybe Label)

-- TODO: To be changed when I figure out what state we need.
{-| The environment:
@envGraph@ - Points a label to the "next" label, or none if it is a
function return.
@envCount@ - Keeps track of local variable names (suffix).
@envGlobal@ - Keeps track of global variable names (suffix).
-}
data Env = Env
  { _envGraph :: Graph
  , _envCount :: Int
  , _envGlobalCount :: Int
  }

$(makeLenses ''Env)

type Convert a = R.ReaderT Signatures (W.WriterT LLVM (ST.State Env)) a

type Signatures = M.Map Ident FunDecl

{- | Every BasicBlock in a function definition has a label, and points
to either another label (another BasicBlock), or Nothing, in which
case it returns from the function.
-}

initEnv :: Env
initEnv = Env M.empty 0 0

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

convert :: J.Prog -> LLVM
convert p
  = ST.evalState (W.execWriterT $ R.runReaderT (convProg p) initSigs) initEnv

convProg :: J.Prog -> Convert ()
convProg (J.Program topDefs) = undefined

-- convTopDefs :: Convert LLVM
-- convTopDefs (topDefs : ts) = 

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
-- * Environment-related helper functions.
--

setCount :: Int -> Convert ()
setCount n = ST.modify $ set envCount n

incrCount :: Convert ()
incrCount = ST.modify $ envCount +~ 1

{- | Return the next unique variable name as an Ident, then
increment the counter.
-}
nextVar :: Convert Ident
nextVar = localId . show <$> ST.gets (view envCount)

--
-- * Various helper functions.
--

-- | Add a global string to the AST.
addGlobalStr :: Ident -> String -> LLVM -> LLVM
addGlobalStr id s llvm = llvm { llvmVarDefs = v : llvmVarDefs llvm }
  where
    v :: VarDef
    v = VarDef id (TArray (length s) (iInt 8)) (SVal $ LString s)
