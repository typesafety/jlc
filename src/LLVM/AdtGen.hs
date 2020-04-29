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
import qualified GHC.Stack as Stack

import LLVM.ADT

import qualified Javalette.Abs as J


type Graph = M.Map Label (Maybe Label)

type Context = M.Map Ident Source

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
  , _envCxts :: [Context]
  }

$(makeLenses ''Env)

type Convert a = R.ReaderT Signatures (ST.State Env) a

type Signatures = M.Map Ident FunDecl

{- | Every BasicBlock in a function definition has a label, and points
to either another label (another BasicBlock), or Nothing, in which
case it returns from the function.
-}

initEnv :: Env
initEnv = Env M.empty 0 0 []

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
      Void (globalId "printString") [Arg (TPointer i8) (localId "s")]
  ]

--
-- * LLVM Generation.
--

convert :: J.Prog -> LLVM
convert p
  = ST.evalState (R.runReaderT (convProg p) initSigs) initEnv

convProg :: J.Prog -> Convert LLVM
convProg = undefined

-- convTopDefs :: [J.TopDef] -> Convert ()
-- convTopDefs [] = return ()
-- convTopDefs (topDef : ts) = do
--   ll <- convTopDef topDef
--   W.tell ll
--   convTopDefs ts

convTopDef :: J.TopDef -> Convert FunDef
convTopDef (J.FnDef jType jId jArgs (J.Block stmts)) = do
  -- Functions have global scope (for now, at least)
  let retType = transType jType
  let id      = transId Global jId
  let args    = map transArg jArgs

  blocks <- convBody stmts
  return $ FunDef retType id args blocks


convBody :: [J.Stmt] -> Convert [BasicBlock]
convBody stmts = do
  pushCxt

  popCxt
  undefined


genInstrs :: J.Stmt -> Convert (Either [BasicBlock] [Instruction])
genInstrs = \case
  J.Empty -> pure $ Right []

  J.BStmt (J.Block stmts) -> error "not defined, how to?"

  -- Due to the preprocessing desugaring step, we can expect
  -- there to be only a single Item per declaration statement,
  -- and no combined declarations and initializations.
  J.Decl jType [J.NoInit jId] ->
    pure $ Right [IAss (transId Local jId) $ IMem $ Alloca (transType jType)]

  J.Ass jId jExpr -> do
    -- Translating the JL expression to LLVM may use multiple instructions.
    instrs <- convExpr jExpr
    -- The final LLVM instruction assigns the value of the expression
    -- to some variable; we want to assign this to the given Id.
    let (inits, [lastInstr]) = splitLast instrs
    pure $ Right $ inits ++ [replaceAss (transId Local jId) lastInstr]

    where
      replaceAss :: Ident -> Instruction -> Instruction
      replaceAss id (IAss _ instr) = IAss id instr

  -- J.Ret expr -> do
  -- VRet
  -- If Expr Stmt
  -- IfElse Expr Stmt Stmt
  -- While Expr Stmt
  -- SExp Expr

  -- We do not handle some cases that are expected to disappear in
  -- preprocessing, so we throw an error if encountered.
  stmt -> error $ "genInstrs: Unexpected Javalette stmt:\n" ++ show stmt

  where
    -- Get the assigned variable from the last instruction of a list.
    -- Crashes if this is not applicable for that particular instruction,
    -- for example return or branching instructions.
    lastBinding :: [Instruction] -> Ident
    lastBinding instrs =
      let (_, [IAss id _]) = splitLast instrs
      in id

    splitLast :: [a] -> ([a], [a])
    splitLast xs = splitAt (length xs - 1) xs

-- TODO
convExpr :: J.Expr -> Convert [Instruction]
convExpr = undefined


--
-- * Functions for translating from Javalette ADT to LLVM ADT.
--

transId :: Scope -> J.Ident -> Ident
transId scope (J.Ident str) = Ident scope str

transArg :: J.Arg -> Arg
transArg (J.Argument jType jId) = Arg (transType jType) (transId Local jId)

transType :: J.Type -> Type
transType = \case
  J.Int    -> i32
  J.Double -> Double
  J.Bool   -> i1
  J.Void   -> Void
  -- Note that this is only usable when setting the type
  -- in function parameters, such as in printString. An actual
  -- string type is of [n x i8]*, that is, an array pointer type.
  J.Str    -> TPointer i8

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
iBit :: Int -> Type
iBit = TNBitInt

-- Shorthands for common integer types.
i32 :: Type
i32 = iBit 32

i8 :: Type
i8 = iBit 8

i1 :: Type
i1 = iBit 1

--
-- * Environment-related helper functions.
--

modTopCxt :: (Context -> Context) -> Convert ()
modTopCxt f = ST.modify $ over envCxts $ \case
  []     -> error errMsg
  x : xs -> f x : xs
  where
    errMsg :: String
    errMsg = mconcat
      [ "modTopCxt: empty context stack\n"
      , "call stack:"
      , Stack.prettyCallStack Stack.callStack
      ]

pushCxt :: Convert ()
pushCxt = ST.modify $ over envCxts (M.empty :)

popCxt :: Stack.HasCallStack => Convert ()
popCxt = ST.modify $ over envCxts $ \case
  []     -> error errMsg
  x : xs -> xs
  where
    errMsg :: String
    errMsg = mconcat
      [ "popCxt: empty context stack\n"
      , "call stack:"
      , Stack.prettyCallStack Stack.callStack
      ]

setCount :: Int -> Convert ()
setCount n = ST.modify $ set envCount n

incrCount :: Convert ()
incrCount = ST.modify $ envCount +~ 1

{- | Return the next unique variable name as an Ident, then
increment the counter.
-}
nextVar :: Convert Ident
nextVar = localId . (varBase ++) . show  <$> ST.gets (view envCount)

{- | The base variable name, onto which the incrementing suffix is
appended. NOTE that this needs to be different from the @varBase@ defined
in the alpha renaming phase, to avoid overlapping and collision of
variables.
-}
varBase :: String
varBase = "lv"

--
-- * Various helper functions.
--

-- | Add a global string to the AST.
addGlobalStr :: Ident -> String -> LLVM -> LLVM
addGlobalStr id s llvm = llvm { llvmVarDefs = v : llvmVarDefs llvm }
  where
    v :: VarDef
    v = VarDef id (TArray (length s) i8) (SVal $ LString s)