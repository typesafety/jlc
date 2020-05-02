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
import Data.Bifunctor (bimap, first, second)
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

type Types = M.Map Ident Type

{-| The carried state:
@stVarCount@ - Counter for local variable names (suffix).
@stGlobal@ - Counter for global variable names (suffix).
@stLabel@ - Counter for label names (suffix).
@stCxt@ - Keeps track of variable names if they've been renamed.
@stGlobalVars@ - Table mapping global variables to string literals.
  Works since Javalette only uses strings as literals, would possibly
  need rework if the language specification changes. The only only
  operations on this map should be _unique_ insertions and reads.
@stTypes@ - Keeps track of types of variables.
-}
data St = St
  { _stGlobalCount :: Int
  , _stVarCount :: Int
  , _stLabelCount :: Int
  , _stCxt :: Context
  , _stGlobalVars :: StringTable
  , _stTypes :: Types
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

isTrI :: Translated -> Bool
isTrI (TrI _) = True
isTrI (TrL _) = False

fromTrI :: Stack.HasCallStack => Translated -> Instruction
fromTrI (TrI i) = i
fromTrI (TrL _) = error "fromTrI: TrL"

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
  [ FunDecl i32     (globalId "readInt")    []
  , FunDecl TDouble (globalId "readDouble") []
  , FunDecl TVoid (globalId "printInt")    [Param i32 (localId "n")]
  , FunDecl TVoid (globalId "printDouble") [Param TDouble (localId "d")]
  , FunDecl TVoid (globalId "printString") [Param (TPointer i8) (localId "s")]
  ]

--
-- * LLVM Generation.
--

convert :: J.Prog -> LLVM
convert p
  = ST.evalState (R.runReaderT (convProg p) initEnv) initSt

convProg :: J.Prog -> Convert LLVM
convProg (J.Program topDefs) =
  R.local (over envSigs $ M.union progDecls) $ do
    undefined
  
  where
    progDecls :: Signatures
    progDecls = M.fromList $ zip (map getId decls) decls
      where
        decls :: [FunDecl]
        decls = map getFunDecl topDefs

        getId :: FunDecl -> Ident
        getId (FunDecl _ id _) = id

-- | Get the LLVM function definition from a JL TopDef construct.
getFunDecl :: J.TopDef -> FunDecl
getFunDecl (J.FnDef jType jId jArgs jBlk) =
  let retType = transType jType
      funId   = transId Global jId
      params  = map transParam jArgs
  in FunDecl retType funId params

convTopDef :: J.TopDef -> Convert FunDef
convTopDef (J.FnDef jType jId jArgs jBlk) = do
  let funId = transId Global jId
  FunDecl t i ps <- fromJust . M.lookup funId <$> R.asks (view envSigs)

  basicBlks <- convBlk jBlk

  return $ FunDef t i ps basicBlks

convBlk :: J.Blk -> Convert [BasicBlock]
convBlk (J.Block stmts) =
  buildBlocks . (TrL (Label "entry") :) . concat <$> mapM convStmt stmts

{- | Given a list of labels and instructions, divide the instructions
into blocks at each label.
-}
buildBlocks :: Stack.HasCallStack => [Translated] -> [BasicBlock]
buildBlocks [] = []
buildBlocks (TrI _ : xs) =
  error "buildBlocks: encountered non-enclosed instruction"
buildBlocks (TrL l : xs) = BasicBlock l instructions : rest
  where
    (instructions, rest) = case first (map fromTrI) . span isTrI $ xs of
      ([], remaining) -> ([unreachable], buildBlocks remaining)
      (xs, remaining) -> (xs,            buildBlocks remaining)

    unreachable :: Instruction
    unreachable = INoAss $ ITerm $ Unreachable

{- | Convert a JL statment into a list of LLVM instructions and labels.
The ordering of the list conserves the semantics of the input program.
-}
convStmt :: Stack.HasCallStack => J.Stmt -> Convert [Translated]
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
    bindType lVar (TPointer lType)
    pure [TrI $ IAss lVar (IMem $ Alloca lType)]

  J.Ass jId jExpr -> do
    (instrs, sid) <- second fromJust <$> convExpr jExpr
    let storeId = transId Local jId
    typeOf (SIdent storeId) >>= \case
      pType@(TPointer typ) -> do
        let storeInstr = INoAss $ IMem $ Store typ sid pType storeId
        pure $ instrs ++ map TrI [storeInstr]

  J.Ret jExpr -> do
    (instrs, sid) <- second fromJust <$> convExpr jExpr
    -- Get the return type of our current fucntion.
    retType <- R.asks (fromJust . view envRetType)
    -- Append a ret instruction to the generated instructions.
    pure $ instrs ++ [TrI $ INoAss $ ITerm $ Ret retType sid]

  J.VRet -> pure [TrI $ INoAss $ ITerm VRet]

  J.SExp jExpr -> fst <$> convExpr jExpr

  J.If jExpr jStmt -> do
    (condInstrs, condId) <- second fromJust <$> convExpr jExpr
    ifLabel <- nextLabel
    endLabel <- nextLabel

    body <- convStmt jStmt

    let brInstr = brCond condId ifLabel endLabel
    let brEnd = brUncond endLabel

    return $ mconcat
      -- Condition
      [ condInstrs
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

    let brInstr = brCond condId ifLabel elseLabel
    let brEnd = brUncond endLabel

    return $ mconcat
      -- Condition
      [ condInstrs
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

    let brInstr = brCond condId bodyLabel endLabel
    let brStart = brUncond condLabel

    return $ mconcat
      -- Condition
      [ [TrL condLabel]
      , condInstrs
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

{- | Convert a Javalette expression to a series of instructions. Return
the instructions and the result as a Source (id or variable). Result is
Nothing if the type of the result was Void.
-}
convExpr
  :: Stack.HasCallStack
  => J.Expr
  -> Convert ([Translated], Maybe Source)
convExpr = \case
  J.EVar jId -> do
    let memId = transId Local jId
    typ <- typeOf (SIdent memId)
    assId <- nextVar
    let instr = IAss assId $ IMem $ Load typ memId

    bindType assId typ >> return ([TrI instr], Just $ SIdent assId)

  J.EApp jId jExprs -> do
    let funId = transId Global jId
    (retType, params) <- lookupFun funId
    (instrss, argVars) <- unzip <$> mapM convExpr jExprs

    let paramTypes = map (\ (Param t _) -> t) params
    let args = zipWith Arg paramTypes (map fromJust argVars)

    case retType of
      TVoid -> do
        let callInstr = INoAss $ IOther $ Call retType funId args
        return (concat instrss ++ [TrI callInstr], Nothing)
      _ -> do
        assId <- nextVar
        let callInstr = IAss assId $ IOther $ Call retType funId args

        bindType assId retType >>
          return (concat instrss ++ [TrI callInstr], Just $ SIdent assId)

  J.EString str -> do
    gId <- addGlobalStr str
    bindType gId (strType str) >> return ([], Just $ SIdent gId)

  -- This should be equivalent
  J.Neg jExpr -> convExpr $ J.EMul (J.ELitInt (-1)) J.Times jExpr

  J.Not jExpr -> do
    (instrs, sid) <- second fromJust <$> convExpr jExpr
    assId <- nextVar
    typ <- typeOf sid

    let ins = IAss assId $ IBitWise $ Xor i1 sid (SVal $ LInt 1)
    bindType assId typ
      >> return (instrs ++ [TrI ins], Just $ SIdent assId)

  J.EMul jE1 jOp jE2 -> do
    (instrs1, sid1, instrs2, sid2, sid1Type, assId) <- convBinOp jE1 jE2

    case (jOp, sid1Type) of
      (J.Times, TNBitInt _) -> do
        let retType = TDouble
        fpVar1 <- nextVar
        fpVar2 <- nextVar
        let inss = mconcat
              [ instrs1
              , instrs2
              , map TrI
                [ IAss fpVar1 $ IOther $ Sitofp sid1Type sid1 retType
                , IAss fpVar2 $ IOther $ Sitofp sid1Type sid2 retType
                , IAss assId
                  $ IArith Fdiv retType (SIdent fpVar1) (SIdent fpVar1)
                ]
              ]
        bindType assId retType >> return (inss, Just $ SIdent assId)

      _ -> do
        let arithOp = getOp jOp sid1Type
        let ins     = IAss assId $ IArith arithOp sid1Type sid1 sid2
        bindType assId sid1Type
          >> return (instrs1 ++ instrs2 ++ [TrI ins], Just $ SIdent assId)

        where
          getOp :: J.MulOp -> Type -> ArithOp
          getOp jOp typ = case (jOp, typ) of
            (J.Times, TDouble)     -> Fmul
            (J.Times, TNBitInt _)  -> Mul
            (J.Div, TDouble)       -> Fdiv
            (J.Mod, TNBitInt _)    -> Srem

  J.EAdd jE1 jOp jE2 -> do
    (instrs1, sid1, instrs2, sid2, sid1Type, assId) <- convBinOp jE1 jE2
    let arithOp = getOp jOp sid1Type
    let ins = IAss assId $ IArith arithOp sid1Type sid1 sid2
    bindType assId sid1Type
      >> return (instrs1 ++ instrs2 ++ [TrI ins], Just $ SIdent assId)

    where
      getOp :: J.AddOp -> Type -> ArithOp
      getOp jOp typ = case (jOp, typ) of
        (J.Plus, TDouble)     -> Fadd
        (J.Plus, TNBitInt _)  -> Add
        (J.Minus, TDouble)    -> Fsub
        (J.Minus, TNBitInt _) -> Sub

  J.ERel jE1 jOp jE2 -> do
    (instrs1, sid1, instrs2, sid2, sid1Type, assId) <- convBinOp jE1 jE2
    let relOpC = getOpC jOp sid1Type
    let ins    = IAss assId $ IOther $ relOpC boolType sid1 sid2

    bindType assId boolType
      >> return (instrs1 ++ instrs2 ++ [TrI ins], Just $ SIdent assId)

    where
      getOpC :: J.RelOp -> Type -> (Type -> Source -> Source -> OtherOp)
      getOpC jOp typ = case (jOp, typ) of
        (J.LTH, TNBitInt _) -> Icmp IC_SLT
        (J.LTH, TDouble)    -> Fcmp FC_OLT
        (J.LE,  TNBitInt _) -> Icmp IC_SLE
        (J.LE,  TDouble)    -> Fcmp FC_OLE
        (J.GTH, TNBitInt _) -> Icmp IC_SGT
        (J.GTH, TDouble)    -> Fcmp FC_OGT
        (J.GE,  TNBitInt _) -> Icmp IC_SGE
        (J.GE,  TDouble)    -> Fcmp FC_OGE
        (J.EQU, TNBitInt _) -> Icmp IC_EQ
        (J.EQU, TDouble)    -> Fcmp FC_OEQ
        (J.NE,  TNBitInt _) -> Icmp IC_NE
        (J.NE,  TDouble)    -> Fcmp FC_ONE

  -- AND and OR have "lazy" behaviour (skip evaluating 2nd operand
  -- if unnecessary).

  J.EAnd jE1 jE2 -> do
    (instrs1, sid1, instrs2, sid2, sid1Type, assId) <- convBinOp jE1 jE2
    op2Label <- nextLabel
    trueLabel <- nextLabel
    falseLabel <- nextLabel
    endLabel <- nextLabel

    allocVar <- nextVar
    let allocate = IAss allocVar $ IMem $ Alloca boolType
    let chk1 = brCond sid1 op2Label falseLabel
    let chk2 = brCond sid2 trueLabel falseLabel
    let storeF = storeBool 0 allocVar
    let storeT = storeBool 1 allocVar
    let gotoEnd = brUncond endLabel
    let loadR = loadBool assId allocVar
    let output = mconcat
          [ wrapI allocate
          , wrapI chk1
          , wrapL op2Label
          , wrapI chk2
          , [ TrL trueLabel
            , TrI storeT
            , TrI gotoEnd
            ]
          , [ TrL falseLabel
            , TrI storeF
            , TrI gotoEnd
            ]
          , [ TrL endLabel
            , TrI loadR
            ]
          ]

    bindType assId boolType
      >> return (output, Just $ SIdent assId)

  J.EOr jE1 jE2 -> do
    (instrs1, sid1, instrs2, sid2, sid1Type, assId) <- convBinOp jE1 jE2
    op2Label <- nextLabel
    trueLabel <- nextLabel
    falseLabel <- nextLabel
    endLabel <- nextLabel

    allocVar <- nextVar
    let allocate = IAss allocVar $ IMem $ Alloca boolType
    let chk1 = brCond sid1 trueLabel op2Label
    let chk2 = brCond sid2 trueLabel falseLabel
    let storeF = storeBool 0 allocVar
    let storeT = storeBool 1 allocVar
    let gotoEnd = brUncond endLabel
    let loadR = loadBool assId allocVar
    let output = mconcat
          [ wrapI allocate
          , wrapI chk1
          , wrapL op2Label
          , wrapI chk2
          , [ TrL trueLabel
            , TrI storeT
            , TrI gotoEnd
            ]
          , [ TrL falseLabel
            , TrI storeF
            , TrI gotoEnd
            ]
          , [ TrL endLabel
            , TrI loadR
            ]
          ]

    bindType assId boolType
      >> return (output, Just $ SIdent assId)

  J.ELitInt n    -> return ([], Just $ SVal $ LInt $ fromIntegral n)
  J.ELitDouble d -> return ([], Just $ SVal $ LDouble d)
  J.ELitTrue     -> return ([], Just $ SVal $ LInt 1)
  J.ELitFalse    -> return ([], Just $ SVal $ LInt 0)

  -- Forgot that we annotated during type checking...
  -- TODO: rewrite code to make use of type annotation.
  J.AnnExp jExpr jType -> convExpr jExpr

  where
    -- Performs converting of subexpressions in binary operations, then
    -- returns the instructions, the variables to which they are assigned,
    -- the result type of the FIRST instruction, and the variable to save
    -- the result to.
    convBinOp
      :: J.Expr
      -> J.Expr
      -> Convert ([Translated], Source, [Translated], Source, Type, Ident)
    convBinOp jE1 jE2 = do
      (instrs1, sid1) <- second fromJust <$> convExpr jE1
      (instrs2, sid2) <- second fromJust <$> convExpr jE2
      typ <- typeOf sid1
      assId <- nextVar
      return (instrs1, sid1, instrs2, sid2, typ, assId)

    wrapI :: Instruction -> [Translated]
    wrapI i = [TrI i]

    wrapL :: Label -> [Translated]
    wrapL l = [TrL l]

    loadBool :: Ident -> Ident -> Instruction
    loadBool to from = IAss to $ IMem $ Load boolType from

    storeBool :: Int -> Ident -> Instruction
    storeBool n id =
      INoAss $ IMem $ Store boolType (SVal $ LInt n) (TPointer boolType) id



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
  J.Double -> TDouble
  J.Bool   -> boolType
  J.Void   -> TVoid
  -- Note that this is only usable when setting the type
  -- in function parameters, such as in printString. An actual
  -- string type is of [n x i8]*, that is, an array pointer type.
  J.Str    -> TPointer i8

--
-- * Helper function for LLVM ADT constructors.
--

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

boolType :: Type
boolType = i1

strType :: String -> Type
strType str = TPointer $ TArray (length str) i8

--
-- * State-related helper functions.
--

-- Given a function ID, return its return type and parameters.
lookupFun :: Stack.HasCallStack => Ident -> Convert (Type, [Param])
lookupFun id = fromJust . M.lookup id <$> R.asks (view envSigs) >>= \case
  FunDecl retType _ params -> return (retType, params)

-- | Set the type for a variable.
bindType :: Stack.HasCallStack => Ident -> Type -> Convert ()
bindType id typ = ST.modify $ over stTypes $ add id typ
  where
    add :: Stack.HasCallStack => Ident -> Type -> Types -> Types
    add i t m = if i `M.notMember` m
                  then M.insert i t m
                  else error "bindType: Attempting to insert existing binding"

-- | Get the type of a Source (variable or literal value).
typeOf :: Source -> Convert Type
typeOf (SIdent id) = (M.! id) <$> ST.gets (view stTypes)
typeOf (SVal lit) = return $ case lit of
  LInt _    -> i32
  LDouble _ -> TDouble
  LNull     -> TNull
  LString s -> strType s

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
