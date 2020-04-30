{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Module for converting an annotated Javalette AST to an
LLVM AST. This is expected to run after type checking and
other preprocessing/optimizations.
-}

-- TODO: Explicit export list
module LLVM.AdtGen where

import Data.Maybe (fromJust)
import Lens.Micro.Platform

import qualified Control.Monad.Reader as R
import qualified Control.Monad.State.Strict as ST
import qualified Control.Monad.Writer.Strict as W
import qualified Data.Map.Strict as M
import qualified GHC.Stack as Stack

import LLVM.ADT

import qualified Javalette.Abs as J

newtype OriginalId = OriginalId Ident

newtype NewId = NewId Ident

type Context = M.Map OriginalId NewId

{-| The environment:
@envVarCount@ - Counter for local variable names (suffix).
@envGlobal@ - Counter for global variable names (suffix).
@envLabel@ - Counter for label names (suffix).
@envCxt@ - Keeps track of variable names if they've been renamed.
-}
data Env = Env
  { _envGlobalCount :: Int
  , _envVarCount :: Int
  , _envLabelCount :: Int
  , _envCxt :: Context
  }

$(makeLenses ''Env)

initEnv :: Env
initEnv = Env 0 0 0 M.empty

{- | Javalette is first translated to this intermediate type, which will
then be assembled into actual basic blocks. This intermediate type
is easier to generate while traversing the Javalette AST.
-}
data Translated
  = TrI Instruction
  | TrL Label

{- | The monad stack keeps track of function signatures and relevant
state when traversing the Javalette AST.
-}
type Convert a = R.ReaderT (Signatures, Maybe Type) (ST.State Env) a

type Signatures = M.Map Ident FunDecl

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
  = ST.evalState (R.runReaderT (convProg p) (initSigs, Nothing)) initEnv

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


{- | Convert a JL statment into a list of LLVM instructions and labels.
The ordering of the list conserves the semantics of the input program.
-}
convStmt :: J.Stmt -> Convert [Translated]
convStmt = \case
  J.Empty -> pure []

  -- Because we have done alpha-renaming, we don't need to worry
  -- about scoping when encountering variable declarations. We can
  -- assume that variables are only declared once, thus there will
  -- be no ambiguities when referencing variables elsewhere.
  J.BStmt (J.Block jStmts) -> concat <$> mapM convStmt jStmts

  -- Due to the preprocessing desugaring step, we can expect
  -- there to be only a single Item per declaration statement,
  -- and no combined declarations and initializations.
  J.Decl jType [J.NoInit jId] -> do
    let lVar = transId Local jId
    let lType = transType jType
    pure [TrI $ IAss lVar (IMem $ Alloca lType)]

  J.Ass jId jExpr -> do
    -- Translating the JL expression to LLVM may use multiple instructions.
    instrs <- convExpr jExpr
    -- The final LLVM instruction assigns the value of the expression
    -- to some variable; we want to assign this to the given Id.
    let (inits, [lastInstr]) = splitLast instrs
    let lId = transId Local jId
    pure . map TrI $ inits ++ [replaceAss lId lastInstr]

    where
      replaceAss :: Ident -> Instruction -> Instruction
      replaceAss id (IAss _ instr) = IAss id instr

  J.Ret jExpr -> do
    instrs <- convExpr jExpr
    let id = lastBinding instrs
    -- Get the return type of our current fucntion.
    retType <- R.asks (fromJust . snd)
    pure . map TrI $ instrs ++ [INoAss $ ITerm $ Ret retType (SIdent id)]

  J.VRet -> pure [TrI $ INoAss $ ITerm VRet]

  J.SExp jExpr -> map TrI <$> convExpr jExpr

  -- J.If jExpr jStmt -> do
  --   condInstrs <- convExpr jExpr
  --   -- let cmpInstr = 


  --   ifLabel <- nextLabel
  --   elseLabel <- nextLabel
  --   body <- convStmt jStmt

  --   -- let br
  --   undefined

  -- IfElse Expr Stmt Stmt
  -- While Expr Stmt

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

newLabel :: String -> Label
newLabel = Label

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

-- TODO: are these even needed
modTopCxt :: (Context -> Context) -> Convert ()
modTopCxt = undefined
-- modTopCxt f = ST.modify $ over envCxts $ \case
--   []     -> error errMsg
--   x : xs -> f x : xs
--   where
--     errMsg :: String
--     errMsg = mconcat
--       [ "modTopCxt: empty context stack\n"
--       , "call stack:"
--       , Stack.prettyCallStack Stack.callStack
--       ]

pushCxt :: Convert ()
pushCxt = undefined -- ST.modify $ over envCxts (M.empty :)

popCxt :: Stack.HasCallStack => Convert ()
popCxt = undefined
-- popCxt = ST.modify $ over envCxts $ \case
--   []     -> error errMsg
--   x : xs -> xs
--   where
--     errMsg :: String
--     errMsg = mconcat
--       [ "popCxt: empty context stack\n"
--       , "call stack:"
--       , Stack.prettyCallStack Stack.callStack
--       ]

setCount :: Lens' Env Int -> Int -> Convert ()
setCount lens n = ST.modify $ set lens n

incrCount :: Lens' Env Int -> Convert ()
incrCount lens = ST.modify $ lens +~ 1

getCount :: Lens' Env Int -> Convert Int
getCount lens = ST.gets $ view lens


next :: Lens' Env Int -> (Lens' Env Int -> Convert a) -> Convert a
next lens makeWith = do
  res <- makeWith lens
  incrCount lens
  return res

{- | Return the next unique label name as an Ident, then
also increment the counter.
-}
nextLabel :: Convert Label
nextLabel = next envLabelCount getLabel
  where
    getLabel :: Lens' Env Int -> Convert Label
    getLabel lens = Label . (labelBase ++) . show <$> getCount lens

{- | Return the next unique global variable name as an Ident, then
also increment the counter.
-}
nextGlobal :: Convert Ident
nextGlobal = next envGlobalCount getGlobal
  where
    getGlobal :: Lens' Env Int -> Convert Ident
    getGlobal lens = globalId . (globalBase ++) . show <$> getCount lens

{- | Return the next unique local variable name as an Ident, then
also increment the counter.
-}
nextVar :: Convert Ident
nextVar = next envVarCount getVar
  where
    getVar :: Lens' Env Int -> Convert Ident
    getVar lens = localId . (varBase ++) . show <$> getCount lens

{- | The base variable names, onto which the incrementing suffix is
appended. NOTE that these need to be different from the @varBase@ defined
in the alpha renaming phase, to avoid overlapping and collision of
variables.
-}
varBase :: String
varBase = "var"

globalBase :: String
globalBase = "gvar"

labelBase :: String
labelBase = "label"

--
-- * Various helper functions.
--

-- | Add a global string to the AST.
addGlobalStr :: Ident -> String -> LLVM -> LLVM
addGlobalStr id s llvm = llvm { llvmVarDefs = v : llvmVarDefs llvm }
  where
    v :: VarDef
    v = VarDef id (TArray (length s) i8) (SVal $ LString s)
