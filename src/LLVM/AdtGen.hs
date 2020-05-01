{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Module for converting an annotated Javalette AST to an
LLVM AST. This is expected to run after type checking and
other preprocessing/optimizations.
-}

-- TODO: Explicit export list
module LLVM.AdtGen where

import Control.Monad (when)
import Data.Bifunctor (second)
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
  deriving (Eq, Ord, Show)

newtype NewId = NewId Ident
  deriving (Eq, Ord, Show)

type Context = M.Map OriginalId NewId

newtype GlobalVar = GlobalVar Ident
  deriving (Eq, Ord, Show)

newtype StringLit = StringLit String
  deriving (Eq, Ord, Show)

type StringTable = M.Map GlobalVar StringLit

type PointerTypes = M.Map Ident Type

{-| The carried state:
@stVarCount@ - Counter for local variable names (suffix).
@stGlobal@ - Counter for global variable names (suffix).
@stLabel@ - Counter for label names (suffix).
@stCxt@ - Keeps track of variable names if they've been renamed.
@stGlobalVars@ - Table mapping global variables to string literals.
  Works since Javalette only uses strings as literals, would possibly
  need rework if the language specification changes. The only only
  operations on this map should be _unique_ insertions and reads.
@stPointerTypes@ - Keeps track of types of pointers. Used _only_ for
  the types of Store and Load instructions (Javalette assignments,
  declarations, and accessing of variables).
-}
data St = St
  { _stGlobalCount :: Int
  , _stVarCount :: Int
  , _stLabelCount :: Int
  , _stCxt :: Context
  , _stGlobalVars :: StringTable
  , _stPointerTypes :: PointerTypes
  }

$(makeLenses ''St)

initSt :: St
initSt = St 0 0 0 M.empty M.empty M.empty

type Signatures = M.Map Ident FunDecl

{- | The environment:
@envSigs@ - Table of mappings from function Idents to their return types and
  argument types. For now, all Idents should be of global scope.
@envRetType@ - When inside a function, this is its return type.
-}
data Env = Env
  { _envSigs :: Signatures
  , _envRetType :: Maybe Type
  }

$(makeLenses ''Env)

initEnv :: Env
initEnv = Env initSigs Nothing
  where
    initSigs :: Signatures
    initSigs = M.fromList $ zip (map getId stdExtFunDefs) stdExtFunDefs
      where
        getId :: FunDecl -> Ident
        getId (FunDecl _ id _) = id

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
type Convert a = R.ReaderT Env (ST.State St) a

--
-- * "Boilerplate"-like content to include in .ll source files.
--

-- | Some external function definitions that should always be included.
stdExtFunDefs :: [FunDecl]
stdExtFunDefs =
  [ FunDecl i32    (globalId "readInt")     []
  , FunDecl Double (globalId "readDouble")  []
  , FunDecl Void   (globalId "printInt")    [Param i32 (localId "n")]
  , FunDecl Void   (globalId "printDouble") [Param Double (localId "d")]
  , FunDecl
      Void (globalId "printString") [Param (TPointer i8) (localId "s")]
  ]

--
-- * LLVM Generation.
--

convert :: J.Prog -> LLVM
convert p
  = ST.evalState (R.runReaderT (convProg p) initEnv) initSt

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
  let params  = map transParam jArgs

  blocks <- convBody stmts
  return $ FunDef retType id params blocks


convBody :: [J.Stmt] -> Convert [BasicBlock]
convBody stmts = do
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
    bindPointerType lVar (TPointer lType)
    pure [TrI $ IAss lVar (IMem $ Alloca lType)]

  J.Ass jId jExpr -> do
    (instrs, id) <- second fromJust <$> convExpr jExpr
    let storeId = transId Local jId
    typeOf storeId >>= \case
      pType@(TPointer typ) -> do
        let storeInstr = INoAss $ IMem $ Store typ (SIdent id) pType storeId
        pure . map TrI $ instrs ++ [storeInstr]

  J.Ret jExpr -> do
    (instrs, id) <- second fromJust <$> convExpr jExpr
    -- Get the return type of our current fucntion.
    retType <- R.asks (fromJust . view envRetType)
    -- Append a ret instruction to the generated instructions.
    pure . map TrI
      $ instrs ++ [INoAss $ ITerm $ Ret retType (SIdent id)]

  J.VRet -> pure [TrI $ INoAss $ ITerm VRet]

  J.SExp jExpr -> map TrI . fst <$> convExpr jExpr

  J.If jExpr jStmt -> do
    (condInstrs, condId) <- second fromJust <$> convExpr jExpr
    ifLabel <- nextLabel
    endLabel <- nextLabel

    body <- convStmt jStmt

    let brInstr = brIdCond condId ifLabel endLabel
    let brEnd = brUncond endLabel

    return $ mconcat
      -- Condition
      [ map TrI condInstrs
      , [TrI brInstr]
      -- If
      , [TrL ifLabel]
      , body
      , [TrI brEnd]
      -- End
      , [TrL endLabel]
      ]

  J.IfElse jExpr jS1 jS2 -> do
    (condInstrs, condId) <- second fromJust <$> convExpr jExpr
    ifLabel <- nextLabel
    elseLabel <- nextLabel
    endLabel <- nextLabel

    ifBody <- convStmt jS1
    elseBody <- convStmt jS2

    let brInstr = brIdCond condId ifLabel elseLabel
    let brEnd = brUncond endLabel

    return $ mconcat
      -- Condition
      [ map TrI condInstrs
      , [TrI brInstr]
      -- If
      , [TrL ifLabel]
      , ifBody
      , [TrI brEnd]
      -- Else
      , [TrL elseLabel]
      , elseBody
      , [TrI brEnd]
      -- End
      , [TrL endLabel]
      ]

  J.While jExpr jStmt -> do
    (condInstrs, condId) <- second fromJust <$> convExpr jExpr
    condLabel <- nextLabel
    bodyLabel <- nextLabel
    endLabel <- nextLabel

    body <- convStmt jStmt

    let brInstr = brIdCond condId bodyLabel endLabel
    let brStart = brUncond condLabel

    return $ mconcat
      -- Condition
      [ [TrL condLabel]
      , map TrI condInstrs
      , [TrI brInstr]
      -- Body
      , [TrL bodyLabel]
      , body
      , [TrI brStart]
      -- End
      , [TrL endLabel]
      ]

  -- We do not handle some cases that are expected to disappear in
  -- preprocessing, so we throw an error if encountered.
  stmt -> error $ "genInstrs: Unexpected Javalette stmt:\n" ++ show stmt

  where
    splitLast :: [a] -> ([a], [a])
    splitLast xs = splitAt (length xs - 1) xs

{- | Convert a Javalette expression to a series of instructions. Return
the instructions and the final variable which the result is assigned to,
if applicable.
-}
convExpr :: J.Expr -> Convert ([Instruction], Maybe Ident)
convExpr = \case
  J.EVar jId -> do
    let memId = transId Local jId
    typ <- typeOf memId
    assId <- nextVar
    let instr = IAss assId $ IMem $ Load typ memId
    return ([instr], Just assId)

  J.EApp jId jExprs -> do
    let funId = transId Global jId
    (retType, params) <- lookupFun funId
    (instrss, argVars) <- unzip <$> mapM convExpr jExprs

    let paramTypes = map (\ (Param t _) -> t) params
    let args = zipWith Arg paramTypes (map fromJust argVars)

    case retType of
      Void -> do
        let callInstr = INoAss $ IOther $ Call retType funId args
        return (concat instrss ++ [callInstr], Nothing)
      _ -> do
        assId <- nextVar
        let callInstr = IAss assId $ IOther $ Call retType funId args
        return (concat instrss ++ [callInstr], Just assId)


  -- EString String
  -- Neg Expr
  -- Not Expr
  -- EMul Expr MulOp Expr
  -- EAdd Expr AddOp Expr
  -- ERel Expr RelOp Expr
  -- EAnd Expr Expr
  -- EOr Expr Expr
  -- AnnExp Expr Type

  -- ELitInt Integer
  -- ELitDouble Double
  -- ELitTrue
  -- ELitFalse

--
-- * Functions for translating from Javalette ADT to LLVM ADT.
--

transId :: Scope -> J.Ident -> Ident
transId scope (J.Ident str) = Ident scope str

transParam :: J.Arg -> Param
transParam (J.Argument jType jId) = Param (transType jType) (transId Local jId)

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

{- | Create a conditional branching instruction where the the conditional
value is a variable.
-}
brIdCond :: Ident -> Label -> Label -> Instruction
brIdCond id = brCond (SIdent id)

-- | Create a conditional branching instruction.
brCond :: Source -> Label -> Label -> Instruction
brCond s l1 l2 = INoAss $ ITerm $ BrCond s l1 l2

-- | Create an unconditional branching instruction.
brUncond :: Label -> Instruction
brUncond l = INoAss $ ITerm $ Br l

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

--
-- * State-related helper functions.
--

-- Given a function ID, return its return type and parameters.
lookupFun :: Stack.HasCallStack => Ident -> Convert (Type, [Param])
lookupFun id = fromJust . M.lookup id <$> R.asks (view envSigs) >>= \case
  FunDecl retType _ params -> return (retType, params)

-- | Set the poitner type for a variable. The variable/type must be a pointer.
bindPointerType :: Stack.HasCallStack => Ident -> Type -> Convert ()
bindPointerType id typ = case typ of
  TPointer _ -> ST.modify $ over stPointerTypes $ ins id typ
  otherType  -> error "bindPointerType: Type not a pointer"
  where
    ins :: Stack.HasCallStack => Ident -> Type -> PointerTypes -> PointerTypes
    ins i t m = if i `M.notMember` m
      then M.insert i t m
      else error "bindPointerType: Attempting to insert existing binding"

-- | Get the pointer type of a variable. The variable must be a pointer.
typeOf :: Ident -> Convert Type
typeOf id = (M.! id) <$> ST.gets (view stPointerTypes)

-- | Add a string as a global variable and return its Ident.
addGlobalStr :: String -> Convert Ident
addGlobalStr str = do
  id <- nextGlobal
  ST.modify $ over stGlobalVars $ M.insert (GlobalVar id) (StringLit str)
  return id

{- | Given the original variable name, return its new (effective) name
as an Ident, then update its mapping to the next unique variable name.
The same as @\ orig -> lookupVar orig >>= \ id -> incrVar orig >> return id@.
-}
lookupIncrVar :: OriginalId -> Convert Ident
lookupIncrVar orig = do
  id <- lookupVar orig
  incrVar orig
  return id

{- | Given the original variable name, update its actual name to the
next unique variable name.
-}
incrVar :: OriginalId -> Convert ()
incrVar origId = do
  newId <- nextVar
  ST.modify $ over stCxt $ M.adjust (const $ NewId newId) origId

{- | Given the original variable name, return its new name as an ident
if it exists (the original has been previously been renamed).
If it does not exist (has not been renamed yet), return the input variable
as an Ident.
-}
lookupVar :: OriginalId -> Convert Ident
lookupVar origId@(OriginalId inId) = do
  cxt <- ST.gets (view stCxt)
  case M.lookup origId cxt of
    Just (NewId outId) -> pure outId
    Nothing            -> pure inId

setCount :: Lens' St Int -> Int -> Convert ()
setCount lens n = ST.modify $ set lens n

incrCount :: Lens' St Int -> Convert ()
incrCount lens = ST.modify $ lens +~ 1

getCount :: Lens' St Int -> Convert Int
getCount lens = ST.gets $ view lens

next :: Lens' St Int -> (Lens' St Int -> Convert a) -> Convert a
next lens makeWith = do
  res <- makeWith lens
  incrCount lens
  return res

{- | Return the next unique label name as an Ident, then
also increment the counter.
-}
nextLabel :: Convert Label
nextLabel = next stLabelCount getLabel
  where
    getLabel :: Lens' St Int -> Convert Label
    getLabel lens = Label . (labelBase ++) . show <$> getCount lens

{- | Return the next unique global variable name as an Ident, then
also increment the counter.
-}
nextGlobal :: Convert Ident
nextGlobal = next stGlobalCount getGlobal
  where
    getGlobal :: Lens' St Int -> Convert Ident
    getGlobal lens = globalId . (globalBase ++) . show <$> getCount lens

{- | Return the next unique local variable name as an Ident, then
also increment the counter.
-}
nextVar :: Convert Ident
nextVar = next stVarCount getVar
  where
    getVar :: Lens' St Int -> Convert Ident
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
