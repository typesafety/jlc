{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Module for converting an annotated Javalette AST to an
LLVM AST. This is expected to run after type checking and
other preprocessing/optimizations, such as desugaring.

-- TODO: Currently does not make use of the type annotations from
--       the type checking phase! Code could probably be vastly simplified
--       with a bit of rewriting.
-- TODO: Currently there is a lot of manual concatenation of Translated
--       objects throughout convExpr. This could probably be abstracted
--       away by using Writer.
-- TODO: Currently the carried State is quite spread out and messy, could
--       possibly be simplified?
-- TODO: (Goes for other modules as well) Errors thrown here should be
--       considered compiler errors; could possibly be propagated upwards
--       and handled more properly (rather than just an error and
--       short stack trace)
-}

module LLVM.AdtGen
       ( convert
       ) where

import Debug.Trace

import Control.Monad (replicateM, void)
import Data.Bifunctor (first, second)
import Data.Functor ((<&>))
import Lens.Micro.Platform

import qualified Control.Monad.Reader as R
import qualified Control.Monad.State.Strict as ST
import qualified Control.Monad.Writer.Strict as W
import qualified Data.Map.Strict as M
import qualified GHC.Stack as Stack

import LLVM.ADT
import qualified LLVM.Shorthands as L

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
  operations on this map should be _unique_ insertions, and reads.
@stTypes@ - Keeps track of types of variables.
-}
data St = St
  { _stGlobalCount :: Int
  , _stVarCount :: Int
  , _stLabelCount :: Int
  , _stGlobalVars :: StringTable
  , _stTypes :: Types
  }

$(makeLenses ''St)

initSt :: St
initSt = St 0 0 0 M.empty M.empty

type Signatures = M.Map Ident FunDecl

{- | The environment:
@envSigs@ - Table of mappings from function Idents to their return types and
  argument types. For now, all Idents should be of global scope.
@envRetType@ - When inside a function, this is its return type.
@envParamIds@ - When inside a function, these are the function's parameters.
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
  deriving (Show)

isTrI :: Translated -> Bool
isTrI (TrI _) = True
isTrI (TrL _) = False

fromTrI :: Stack.HasCallStack => Translated -> Instruction
fromTrI (TrI i) = i
fromTrI (TrL _) = error "fromTrI: TrL"

{- | The monad stack keeps track of function signatures and relevant
state when traversing the Javalette AST.
-}
type Convert a = R.ReaderT Env (W.WriterT [Translated] (ST.State St)) a

runConvert :: Env -> St -> Convert a -> a
runConvert e s m = fst $ ST.evalState (W.runWriterT (R.runReaderT m e)) s

--
-- * "Boilerplate"-like content to include in .ll source files.
--

-- | Some external function definitions that should always be included.
stdExtFunDefs :: [FunDecl]
stdExtFunDefs =
  [ FunDecl i32     (globalId "readInt")     []
  , FunDecl TDouble (globalId "readDouble")  []
  , FunDecl TVoid   (globalId "printInt")    [i32]
  , FunDecl TVoid   (globalId "printDouble") [TDouble]
  , FunDecl TVoid   (globalId "printString") [TPointer i8]
  ]

--
-- * LLVM Generation.
--

-- | Convert a Javalette AST to a LLVM AST.
convert :: J.Prog -> LLVM
convert = runConvert initEnv initSt . convProg

convProg :: J.Prog -> Convert LLVM
convProg (J.Program topDefs) =
  -- Add the function signatures from the JL definitions.
  R.local (over envSigs $ M.union progDecls) $ do
    funDefs <- convTopDefs topDefs -- TODO: RESET CERTAIN STATE AFTER EACH DEF
    varDefs <- map toVarDef . M.toList <$> ST.gets (view stGlobalVars)
    let funDecls = stdExtFunDefs
    return $ LLVM 
      { llvmTypeDefs = []  -- Not implemented for now.
      , llvmVarDefs  = varDefs
      , llvmFunDefs  = funDefs
      , llvmFunDecls = funDecls
      }

  where
    -- Only supports string literals.
    toVarDef :: (GlobalVar, StringLit) -> VarDef
    toVarDef (GlobalVar id, StringLit str) =
        -- String variables have type [n x i8]*, here we write [n x i8]
        -- though, since it's the type of the actual string constant.
      let TPointer strConstType = strType str
      in VarDef id strConstType (SVal (strType str) (LString str))

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
      pTypes  = map (\ (J.Argument t _) -> transType t) jArgs
  in FunDecl retType funId pTypes

convTopDefs :: [J.TopDef] -> Convert [FunDef]
convTopDefs []       = return []
convTopDefs (d : ds) = do
  fd <- convTopDef d
  clearSt
  fds <- convTopDefs ds
  return $ fd : fds
  where
    -- Reset parts of the state between function definitions.
    clearSt :: Convert ()
    clearSt = ST.modify
      $ set stVarCount 0
      . set stLabelCount 0
      . set stTypes M.empty

convTopDef :: J.TopDef -> Convert FunDef
convTopDef (J.FnDef jType jId jArgs jBlk) = do
  let retType = transType jType
  let funId   = transId Global jId

  -- In Javalette, function parameters behave the same as any other
  -- declared variable. In LLVM however, our "normal" variables are
  -- pointers to their value, while the function parameters are not.
  -- Because of this, for each function parameter, we insert a
  -- pointer allocation to the beginning of each instruction, and
  -- store the parameter value to memory. Any references to the
  -- parameter in the function body can then use the pointer variable
  -- instead. Performance-wise, it should be OK, if the optimizer
  -- is run.
  (params, extraInstrs) <- fixParams jArgs

  -- Set the function's return type in the environment.
  basicBlks <- R.local (set envRetType (Just retType)) $ convBlk jBlk
  let bbinj f (BasicBlock l is : bbs) = BasicBlock l (f is) : bbs

  return $ FunDef retType funId params (bbinj (extraInstrs ++) basicBlks)

  where
    fixParams :: [J.Arg] -> Convert ([Param], [Instruction])
    fixParams jArgs = do
      -- Create the parameters and extra instructions.
      pis <- paramsInstrs 0 jArgs
      -- We must not forget to add the types of our new  variables
      -- to the state.
      ST.modify $ set stTypes (M.fromList $ map (jArgToIdType Local) jArgs)
      return pis

    paramsInstrs :: Int -> [J.Arg] -> Convert ([Param], [Instruction])
    paramsInstrs count [] = return ([], [])
    paramsInstrs count (J.Argument jType jId : xs) = do
      -- Create the parameter with a new name.
      let lType        = transType jType
      let newId        = Ident Local ('p' : show count)
      let renamedParam = Param lType newId
      -- Create instructions for storing the parameter value in memory.
      let origId     = transId Local jId
      let allocInstr = IAss origId (IMem $ Alloca lType)
      let storeInstr =
            INoAss $ IMem $ Store lType (SIdent newId) (TPointer lType) origId
      (ps, is) <- paramsInstrs (count + 1) xs
      return (renamedParam : ps, [allocInstr, storeInstr] ++ is)

    jArgToIdType :: Scope -> J.Arg -> (Ident, Type)
    jArgToIdType scope (J.Argument jType jId) =
      (transId scope jId, TPointer $ transType jType)

convBlk :: J.Blk -> Convert [BasicBlock]
convBlk (J.Block stmts) =
  let blockify = map tieUp . buildBlocks . (TrL (Label "entry") :)
  in blockify . snd <$> W.listen (mapM_ convStmt stmts)
  where
    -- Add an "unreachable" instruction to the end of a basic block if
    -- it does not end in a terminator instruction.
    tieUp :: Stack.HasCallStack => BasicBlock -> BasicBlock
    tieUp inBlk@(BasicBlock l is) = case last is of
      INoAss (ITerm _) -> inBlk
      _                -> BasicBlock l (is ++ [INoAss $ ITerm Unreachable])

    -- Given a list of labels and instructions, divide the instructions
    -- into blocks at each label.
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
        unreachable = INoAss $ ITerm Unreachable

{- | Convert a JL statment into a list of LLVM instructions and labels.
The ordering of the list conserves the semantics of the input program.
-}
convStmt :: Stack.HasCallStack => J.Stmt -> Convert ()
convStmt s = case s of
  J.Empty -> return ()

  -- Because we have done alpha-renaming, we don't need to worry
  -- about scoping when encountering variable declarations. We can
  -- assume that variables are only declared once, thus there will
  -- be no ambiguities when referencing variables elsewhere.
  J.BStmt (J.Block jStmts) -> mapM_ convStmt jStmts

  -- Due to the preprocessing desugaring step, we can expect
  -- there to be only a single Item per declaration statement,
  -- and no combined declarations and initializations.
  J.Decl jType [J.NoInit jId] -> do
    let lId = transId Local jId
    let lType = transType jType

    bindType lId (toPtr lType)
    tellI $ lId `L.alloca` lType

  J.Ass jId jExpr -> do
    srcId <- convExpr jExpr
    let storeId = transId Local jId
    ptrType <- typeOf (SIdent storeId)
    let TPointer valType = ptrType
    tellI $ L.store valType srcId ptrType storeId

  J.Ret jExpr -> do
    srcId <- convExpr jExpr
    retType <- R.asks (fromJust . view envRetType)
    tellI $ L.ret retType srcId

  J.VRet -> tellI L.vret

  J.SExp jExpr -> void $ convExpr jExpr

  J.If eCond sBody -> do
    ifLabel <- nextLabel
    endLabel <- nextLabel

    condRes <- convExpr eCond
    tellI $ L.brCond condRes ifLabel endLabel
    tellL ifLabel
    convStmt sBody
    tellI $ L.brUncond endLabel
    tellL endLabel

  J.IfElse eCond sT sF -> do
    ifLabel <- nextLabel
    elseLabel <- nextLabel
    endLabel <- nextLabel

    condRes <- convExpr eCond
    tellI $ L.brCond condRes ifLabel elseLabel
    tellL ifLabel
    convStmt sT
    tellI $ L.brUnCond endLabel
    tellL elseLabel
    convStmt sF
    tellI $ L.brUnCond endLabel
    tellL endLabel

  J.While eCond sBody -> do
    condLabel <- nextLabel
    bodyLabel <- nextLabel
    endLabel <- nextLabel

    -- Need to add a branch instruction to get from the previous
    -- basic block to here, due to no falling through blocks.
    tellI $ L.brUncond condLabel
    tellL condLabel
    condRes <- convExpr eCond
    tellI $ L.brCond condRes bodyLabel endLabel
    tellL bodyLabel
    convStmt sBody
    tellI $ L.brUncond condLabel
    tellL endLabel

  -- We do not handle some cases that are expected to disappear in
  -- preprocessing, so we throw an error if encountered.
  stmt -> error $ "genInstrs: Unexpected Javalette stmt:\n" ++ show stmt

{- | Convert a Javalette expression to a series of instructions. Return
the result as a Source, containing the variable in which the result is
stored, or the literal value. Result is a null literal if neither
of the earlier applies.
-}
convExpr :: Stack.HasCallStack => J.Expr -> Convert Source
convExpr e = case e of
  J.EVar jId -> do
    let lId = transId Local jId
    ptrType <- typeOf $ SIdent lId
    -- Type when accessing a variable must be a pointer type, crash otherwise.
    let TPointer valType = ptrType 

    assId <- nextVar
    tellI $ (assId `L.load` valType) ptrType lId

    bindRetId assId valType

  -- Hardcoded case for printString.
  J.EApp jId@(J.Ident "printString") [jExpr] -> do
    let funId = transId Global jId

    arrPtr <- convExpr jExpr
    -- arrPtrV is a variable of type [n x i8]*
    arrPtrType <- typeOf arrPtr
    let (TPointer arrType) = t
    ptr <- nextVar

    let args = [ (arrPtrType, arrPtr)
               , (L.i32, L.srcI32 0)
               , (L.i32, L.srcI32 0)
               ]
    tellI $ (ptr `L.getelementptr` arrType) args
    let funArgs = [Arg (L.toPtr L.i8) (SIdent ptr)]
    tellI $ L.callV funId funArgs

    retLit LNull

  J.EApp jId jExprs -> do
    let funId = transId Global jId

    (retType, paramTypes) <- lookupFun funId
    paramSrcs <- mapM convExpr jExprs
    let args = zipWith Arg paramTypes paramSrcs

    case retType of
      TVoid -> do
        tellI $ L.callV funId args
        retLit LNull
      _ -> do
        assId <- nextVar
        tellI $ (assId `L.call`) retType funId args
        bindRetId assId retType

  J.EString str -> addGlobalStr str >>= \ gId -> bindRetId gId (L.strType str)

  J.Neg jExpr -> do
    assId <- nextVar
    srcId <- convExpr jExpr
    typeOf srcId >>= \case
      TNBitInt 32 -> tellI $ (assId `L.mul` L.i32) srcId (L.srcI32 (-1))
      TDouble     -> tellI $ (assId `L.fmul` TDouble) srcId (L.srcD (-1))
    bindRetId assId

  J.Not jExpr -> do
    assId <- nextVar
    srcId <- convExpr jExpr
    retType <- typeOf srcId
    tellI $ (assId `L.xor` retType) srcId L.srcTrue
    bindRetId assId

  J.EAdd jE1 jOp jE2 -> convArithOp jE1 jE2 jOp getOp
    where
      getOp (Left jOp) retType = case (jOp, retType) of 
          (J.Plus,  TNBitInt _) -> Add
          (J.Plus,  TDouble)    -> Fadd
          (J.Minus, TNBitInt _) -> Sub
          (J.Minus, TDouble)    -> Fsub

  J.EMul jE1 jOp jE2 -> convArithOp jE1 jE2 jOp getOp
    where
      getOp (Right jOp) retType = case (jOp, retType) of 
        (J.Times, TNBitInt _) -> Mul
        (J.Times, TDouble)    -> Fmul
        (J.Div,   TNBitInt _) -> Sdiv
        (J.Div,   TDouble)    -> Fdiv
        (J.Mod,   TNBitInt _) -> Srem


  -- TODO HERE:
  J.ERel jE1 jOp jE2 -> do
    (instrs1, sid1, instrs2, sid2, sid1Type, assId) <- convBinOp jE1 jE2
    let relOpC = getOpC jOp sid1Type
    let ins    = IAss assId $ IOther $ relOpC sid1Type sid1 sid2

    bindType assId bool
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

  -- TODO: AND and OR shoudl have "lazy" behaviour (skip evaluating
  -- 2nd operand if unnecessary).
  -- TODO: EAnd/EOr are very similar; generalize?

  J.EAnd jE1 jE2 -> do
    (inss1, sid1) <- second fromJust <$> convExpr jE1
    (inss2, sid2) <- second fromJust <$> convExpr jE2

    -- Needed labels
    lbls <- replicateM 4 nextLabel
    let [lEvalSnd, lWriteT, lWriteF, lEnd] = lbls

    -- (Start) Allocate memory for storing the result of the expression.
    resMem <- nextVar
    let insResMem = IAss resMem (IMem $ Alloca bool)
    -- Evaluate the first expression and branch.
    e1res <- nextVar
    let ins1   = IAss e1res (IBitwise $ And i1 sid1 srcTrue)
    let insBr1 = brCond (SIdent e1res) lEvalSnd lWriteF

    -- (lEvalSnd) Evaluate the second expression and branch.
    e2res <- nextVar
    let ins2   = IAss e2res (IBitwise $ And i1 sid2 srcTrue)
    let insBr2 = brCond (SIdent e2res) lWriteT lWriteF

    -- (lWriteT) Store true.
    let insWriteT = bWrite True  resMem
    -- (lWriteF) Store false.
    let insWriteF = bWrite False resMem
    -- Jump to end.
    let insDone = brUncond lEnd

    -- (lEnd) Load the result.
    res <- nextVar
    let insLoadRes = IAss res (IMem $ Load i1 (toPtr i1) resMem)

    -- List of Translated.
    let output = mconcat
          [ inss1
          , [ TrI insResMem, TrI ins1, TrI insBr1 ]
          , TrL lEvalSnd : inss2
          , [ TrI ins2, TrI insBr2 ]
          , [ TrL lWriteT, TrI insWriteT, TrI insDone
            , TrL lWriteF, TrI insWriteF, TrI insDone
            , TrL lEnd,    TrI insLoadRes
            ]
          ]
    bindType res bool >> return (output, Just (SIdent res))

    where
      -- Write true (1) or false (0) to a given Ident, which must be a i1*.
      bWrite :: Bool -> Ident -> Instruction
      bWrite b id =
        let v = if b then srcLitN i1 1 else srcLitN i1 0
        in INoAss $ IMem $ Store i1 v (toPtr i1) id

  J.EOr jE1 jE2 -> do
    (inss1, sid1) <- second fromJust <$> convExpr jE1
    (inss2, sid2) <- second fromJust <$> convExpr jE2

    -- Needed labels
    lbls <- replicateM 4 nextLabel
    let [lEvalSnd, lWriteT, lWriteF, lEnd] = lbls

    -- (Start) Allocate memory for storing the result of the expression.
    resMem <- nextVar
    let insResMem = IAss resMem (IMem $ Alloca bool)
    -- Evaluate the first expression and branch.
    e1res <- nextVar
    let ins1   = IAss e1res (IBitwise $ And i1 sid1 srcTrue)
    let insBr1 = brCond (SIdent e1res) lWriteT lEvalSnd

    -- (lEvalSnd) Evaluate the second expression and branch.
    e2res <- nextVar
    let ins2   = IAss e2res (IBitwise $ And i1 sid2 srcTrue)
    let insBr2 = brCond (SIdent e2res) lWriteT lWriteF

    -- (lWriteT) Store true.
    let insWriteT = bWrite True  resMem
    -- (lWriteF) Store false.
    let insWriteF = bWrite False resMem
    -- Jump to end.
    let insDone = brUncond lEnd

    -- (lEnd) Load the result.
    res <- nextVar
    let insLoadRes = IAss res (IMem $ Load i1 (toPtr i1) resMem)

    -- List of Translated.
    let output = mconcat
          [ inss1
          , [ TrI insResMem, TrI ins1, TrI insBr1 ]
          , TrL lEvalSnd : inss2
          , [ TrI ins2, TrI insBr2 ]
          , [ TrL lWriteT, TrI insWriteT, TrI insDone
            , TrL lWriteF, TrI insWriteF, TrI insDone
            , TrL lEnd,    TrI insLoadRes
            ]
          ]
    bindType res bool >> return (output, Just (SIdent res))

    where
      -- Write true (1) or false (0) to a given Ident, which must be a i1*.
      bWrite :: Bool -> Ident -> Instruction
      bWrite b id =
        let v = if b then srcLitN i1 1 else srcLitN i1 0
        in INoAss $ IMem $ Store i1 v (toPtr i1) id

  J.ELitInt n    -> return ([], Just $ srcI32 (fromIntegral n))
  J.ELitDouble d -> return ([], Just $ srcLitD d)
  J.ELitTrue     -> return ([], Just srcTrue)
  J.ELitFalse    -> return ([], Just srcFalse)

  -- Forgot that we annotated during type checking...
  -- TODO: rewrite code to make use of type annotation.
  J.AnnExp jExpr jType -> convExpr jExpr

  where
    convArithOp
      :: J.Expr
      -> J.Expr
      -> Either J.AddOp J.MulOp
      -> (Either J.AddOp J.MulOp -> Type -> ArithOp)
      -> Convert Source
    convArithOp jE1 jE2 jOp getOp = do
      srcId1 <- convExpr jE1
      srcId2 <- convExpr jE1
      retType <- typeOf srcId1
      assId <- nextVar
      tellI $ L.arith (getOp jOp retType) assId retType srcId1 srcId2
      bindRetId assId

    bindRetId :: Ident -> Type -> Convert Source
    bindRetId id typ = bindType id typ >> retId id

    retId :: Ident -> Convert Source
    retId id = return $ SIdent id

    retLit :: Type -> Lit -> Convert Source
    retLit typ lit = return $ SVal typ lit


--
-- * Functions for translating from Javalette ADT to LLVM ADT.
--

transId :: Scope -> J.Ident -> Ident
transId scope (J.Ident str) = Ident scope str

transParam :: J.Arg -> Param
transParam (J.Argument jType jId) = Param (transType jType) (transId Local jId)

transType :: Stack.HasCallStack => J.Type -> Type
transType = \case
  J.Int    -> i32
  J.Double -> TDouble
  J.Bool   -> bool
  J.Void   -> TVoid
  J.Str    -> error "String type needs special care"

--
-- * Helper function for LLVM ADT constructors.
--


--
-- * State-related helper functions.
--

-- Given a function ID, return its return type and parameters.
lookupFun :: Stack.HasCallStack => Ident -> Convert (Type, [Type])
lookupFun id = unsafeLookup id <$> R.asks (view envSigs) >>= \case
  FunDecl retType _ pTypes -> return (retType, pTypes)

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
typeOf (SIdent id) = unsafeLookup id <$> ST.gets (view stTypes)
typeOf (SVal typ lit) = return $ case lit of
  LInt _    -> typ
  LDouble _ -> TDouble
  LNull     -> TNull
  LString s -> strType s

-- | Add a string as a global variable and return its Ident.
addGlobalStr :: String -> Convert Ident
addGlobalStr str = do
  id <- nextGlobal
  ST.modify $ over stGlobalVars $ M.insert (GlobalVar id) (StringLit str)
  return id

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
-- * Other helper functions
--

fromJust :: Stack.HasCallStack => Maybe a -> a
fromJust Nothing  = error "fromJust: Nothing"
fromJust (Just a) = a

unsafeLookup :: (Stack.HasCallStack, Ord k, Show k) => k -> M.Map k a -> a
unsafeLookup k m = case M.lookup k m of
  Just v  -> v
  Nothing -> error $ "unsafeLookup: key `" ++ show k
                   ++ "` could not be found in map"

tellI :: Instruction -> Convert ()
tellI i = W.tell [TrI i]

tellL :: Label -> Convert ()
tellL l = W.tell [TrL l]
