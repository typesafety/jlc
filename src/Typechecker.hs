{-# LANGUAGE LambdaCase #-}

module Typechecker where

import Debug.Trace

import           Control.Applicative ((<|>))
import           Control.Monad (unless, when, zipWithM)
import qualified Control.Monad.Except as E
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State as ST
import           Data.Containers.ListUtils (nubOrd)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)

import Errors (Error (..))
import Javalette.Abs

type Typecheck a = R.ReaderT Signatures (ST.StateT Env (E.Except Error)) a

runTypecheck :: Typecheck a -> Either Error a
runTypecheck t =
  E.runExcept $ ST.evalStateT (R.runReaderT t emptySig) emptyEnv

type Signatures = M.Map Ident ([Type], Type)

type Context = M.Map Ident Type

type Env = [Context]

emptySig :: Signatures
emptySig = M.fromList
  [ (Ident "readInt",     ([],       Int))
  , (Ident "readDouble",  ([],       Double))
  , (Ident "printInt",    ([Int],    Void))
  , (Ident "printDouble", ([Double], Void))
  , (Ident "printString", ([Str],    Void))
  ]

emptyCxt :: Context
emptyCxt = M.empty

emptyEnv :: Env
emptyEnv = []

-- | Entry point, typecheck and annotate a parsed program.
typecheck :: Prog -> Typecheck Prog
typecheck (Program topDefs) =
  getSigs topDefs >>= \ sigs -> R.local (M.union sigs) $ do
    checkMain
    annotatedTopDefs <- checkDefs topDefs
    return $ Program annotatedTopDefs

-- * Functions for typechecking and annotating a program.

checkDefs :: [TopDef] -> Typecheck [TopDef]
checkDefs = mapM checkDef

checkDef :: TopDef -> Typecheck TopDef
checkDef (FnDef typ id args (Block stmts)) = do
  -- First, add the function return type as the only binding in
  -- the bottom context of the stack. This bottom context will contain
  -- only this binding and serves only to store the return type.
  pushCxt
  bindType id typ

  -- Then, push another context onto the stack and bind the types
  -- of the function arguments.
  pushCxt
  bindArgs args

  -- Ensure reachable return statement(s) for non-void functions.
  -- It is enough to check that return statements exists and are
  -- reachable; any type-incorrect returnts will be caught in the
  -- later call to `checkStmts`.
  unless (typ == Void || reachableRet stmts) $ err $ MissingReturnError id

  -- Typecheck the statements in the function body, annotating expressions.
  annotated <- checkStmts stmts

  -- Finally, when finished, pop the earlier two contexts off the stack
  -- and return the annotated TopDef.
  popCxt
  popCxt

  return $ FnDef typ id args (Block annotated)

-- | Returns True if a list of statements has guaranteed reachable
-- return statements.
reachableRet :: [Stmt] -> Bool
reachableRet []       = False
reachableRet (s : ss) = case s of
  Ret _                 -> True
  BStmt (Block stmts)   -> reachableRet stmts  || reachableRet ss
  If ELitTrue stmt      -> reachableRet [stmt] || reachableRet ss
  While ELitTrue stmt   -> reachableRet [stmt] || reachableRet ss
  IfElse exp s1 s2
    | exp == ELitTrue  -> reachableRet [s1] || reachableRet ss
    | exp == ELitFalse -> reachableRet [s2] || reachableRet ss
    | otherwise        -> (reachableRet [s1] && reachableRet [s2])
                          || reachableRet ss

  _ -> reachableRet ss

checkStmts :: [Stmt] -> Typecheck [Stmt]
checkStmts []       = return []
checkStmts (s : ss) = do
  s'  <- checkStmt s
  ss' <- checkStmts ss
  return $ s' : ss'

checkStmt :: Stmt -> Typecheck Stmt
checkStmt = \case
  Empty -> return Empty

  BStmt (Block stmts) -> do
    pushCxt
    checkedStmts <- checkStmts stmts
    popCxt
    return $ BStmt (Block checkedStmts)

  Decl typ items -> do
    checkedItems <- mapM (checkItem typ) items
    return $ Decl typ checkedItems

    where
      checkItem :: Type -> Item -> Typecheck Item
      checkItem expected item = case item of
        NoInit id   -> do
          bindType id expected
          return item
        Init id exp -> do
          annExp <- annotateWithType expected exp
          bindType id expected
          return $ Init id annExp

  Ass id exp -> do
    varType <- lookupVar id
    annExp <- annotateWithType varType exp
    return $ Ass id annExp

  Incr id@(Ident name) -> lookupVar id >>= \case
    Int -> return $ Incr id
    typ -> err $ Error $ "Incrementing (++) " ++ name ++ " requires type"
                       ++ "int, but instead got type" ++ show typ

  Decr id@(Ident name) -> lookupVar id >>= \case
    Int -> return $ Decr id
    typ -> err $ Error $ "Decrementing (--) " ++ name ++ " requires type"
                       ++ "int, but instead got type" ++ show typ

  Ret exp -> do
    (annExp, typ) <- annotate2 exp
    checkRet typ
    return $ Ret annExp

  VRet -> do
    checkRet Void
    return VRet

  If exp stmt -> do
    annExp <- annotateWithType Bool exp
    checkedStmt <- checkStmt stmt
    return $ If annExp checkedStmt

  IfElse exp s1 s2 -> do
    annExp <- annotateWithType Bool exp
    checkedS1 <- checkStmt s1
    checkedS2 <- checkStmt s2
    return $ IfElse annExp checkedS1 checkedS2

  While exp stmt -> do
    annExp <- annotateWithType Bool exp
    checkedStmt <- checkStmt stmt
    return $ While annExp checkedStmt

  SExp exp -> do
    (annExp, eType) <- annotate2 exp
    if eType `tEq` Void
      then return $ SExp annExp
      else err $ NonVoidSExpError exp eType

  where
    -- Given a type, check that the return type of the current
    -- function has the same type, and throw an error if not.
    checkRet :: Type -> Typecheck ()
    checkRet t = do
      (id, retType) <- getRet
      if t `tEq` retType
        then return ()
        else err $ ReturnError id retType t

-- | Checks for the existence of main(), and that it has the
-- correct arguments and return type.
checkMain :: Typecheck ()
checkMain = R.reader (M.lookup (Ident "main")) >>= \case
  Nothing -> err $ Error "Missing main() function"

  Just (argTypes, retType)
    | retType /= Int ->
      err $ Error "main() does not not have return type `int`"

    | not $ null argTypes ->
      err $ Error "main() must have zero arguments"

    | otherwise -> return ()

-- | Run @annotate@ but throw an error if the inferred type does not
-- match the given type.
annotateWithType :: Type -> Expr -> Typecheck Expr
annotateWithType expected exp = annotate exp >>= \case
  annExp@(AnnExp e t)
    | expected `tEq` t -> return annExp
    | otherwise -> err $ ExpError exp [expected] t
  _ -> error "annotateWithType: `annotate` did not return an AnnExp expression"

-- | Like @annotate@, but also return the type of the expression.
annotate2 :: Expr -> Typecheck (Expr, Type)
annotate2 exp = annotate exp >>= \case
  AnnExp e t -> return (e, t)
  _ -> error "annotate2: `annotate` did not return an AnnExp expression"

-- | Given an expression, infer its type and return its annotated version.
-- This also annotates any sub-expressions.
annotate :: Expr -> Typecheck Expr
annotate topExp = do
  (exp', eType) <- ann
  return $ AnnExp exp' eType

  where
    ann :: Typecheck (Expr, Type)
    ann = case topExp of
      ELitInt _    -> return (topExp, Int)
      ELitDouble _ -> return (topExp, Double)
      ELitTrue     -> return (topExp, Bool)
      ELitFalse    -> return (topExp, Bool)
      EString _    -> return (topExp, Str)

      EApp id exps -> R.reader (M.lookup id) >>= \case
        Nothing -> err $ SymbolError id
        Just (argTypes, retType) -> do
          -- Throw an error if number of arguments do not match the
          -- number of parameters to the function.
          when (length argTypes /= length exps)
            $ err $ NumArgsError id (length argTypes) (length exps)

          annExps <- zipWithM annotateWithType argTypes exps

          return (EApp id annExps, Fun retType argTypes)

      EVar id -> do
        typ <- lookupVar id
        return (topExp, typ)

      Neg exp -> do
        (annExp, eType) <- annotate2 exp
        let allowed = [Int, Double]
        if eType `okType` allowed
          then return (Neg annExp, eType)
          else err $ ExpError exp allowed eType

      Not exp -> do
        (annExp, eType) <- annotate2 exp
        if eType `okType` [Bool]
          then return (Neg annExp, eType)
          else err $ ExpError exp [Bool] eType

      EMul e1 op e2 -> do
        (annE1, annE2, eType) <- annBinOp allowedOperands Nothing e1 e2
        return (EMul annE1 op annE2, eType)
        where
          allowedOperands :: [Type]
          allowedOperands
            | op `elem` [Times, Div] = [Int, Double]
            | op == Mod              = [Int]

      EAdd e1 op e2 -> do
        (annE1, annE2, eType) <- annBinOp [Int, Double] Nothing e1 e2
        return (EAdd annE1 op annE2, eType)

      ERel e1 op e2 -> do
        (annE1, annE2, eType) <- annBinOp allowedOperands (Just Bool) e1 e2
        return (ERel annE1 op annE2, eType)
        where
          allowedOperands :: [Type]
          allowedOperands
            | op `elem` [EQU, NE]          = [Int, Double, Bool]
            | op `elem` [LTH, LE, GTH, GE] = [Int, Double]

      EAnd e1 e2 -> do
        (annE1, annE2, eType) <- annBinOp [Bool] (Just Bool) e1 e2
        return (EAnd annE1 annE2, eType)

      EOr e1 e2 -> do
        (annE1, annE2, eType) <- annBinOp [Bool] (Just Bool) e1 e2
        return (EOr annE1 annE2, eType)

      AnnExp{} ->
        error "annotate: attempting to annotate an annotated expression"

      where
        -- Checks and annotates binary operations. Returns the annotated
        -- expressions and their types. Assumes that type casting is forbidden.
        annBinOp :: [Type]      -- Allowed types for operands
                 -> Maybe Type  -- Possible return type
                 -> Expr        -- Left operand
                 -> Expr        -- Right operand
                 -> Typecheck (Expr, Expr, Type)
        annBinOp allowedTypes mbyRetType e1 e2 = do
          (annE1, type1) <- annotate2 e1
          (annE2, type2) <- annotate2 e2
          if type1 `okType` allowedTypes && type1 `tEq` type2
            then return (annE1, annE2, fromMaybe type1 mbyRetType)
            else err $ ExpError e2 allowedTypes type2

--
-- * Functions related to modifying the context.
--

bindArgs :: [Arg] -> Typecheck ()
bindArgs args = ST.get >>= \case
  []     -> error "bindArgs: empty context stack"
  c : cs -> do
    let c' = M.union c . M.fromList . map argToTuple $ args
    ST.put $ c' : cs

  where
    argToTuple :: Arg -> (Ident, Type)
    argToTuple (Argument typ id) = (id, typ)

bindType :: Ident -> Type -> Typecheck ()
bindType id typ = ST.gets (M.lookup id . head) >>= \case
  Just _  -> err $ DuplicateDeclError id
  Nothing -> updateCxt (M.insert id typ)

updateCxt :: (Context -> Context) -> Typecheck ()
updateCxt f = ST.modify (\ (c : cs) -> f c : cs)

pushCxt :: Typecheck ()
pushCxt = ST.modify (M.empty :)

popCxt :: Typecheck ()
popCxt = ST.get >>= \case
  []       -> error "popCxt: empty context stack"
  (x : xs) -> ST.put xs

-- | Return the current function's identifier and return type.
-- Assumes that the context stack is implemented such that the bottom
-- context contains only a single binding (the one of the current function).
getRet :: Typecheck (Ident, Type)
getRet = ST.gets $ head . M.toList . last

--
-- * Various helper functions.
--

-- | Given an identifier, check if it exists in the current context stack.
-- If so, return the type of its topmost occurrence, and throw an
-- error otherwise.
lookupVar :: Ident -> Typecheck Type
lookupVar id = ST.get >>= \ cxts -> case lookupVar' id cxts of
  Just typ -> return typ
  Nothing  -> err $ SymbolError id

  where
    lookupVar' :: Ident -> Env -> Maybe Type
    lookupVar' _ []        = Nothing
    lookupVar' id (c : cs) = M.lookup id c <|> lookupVar' id cs

-- | Get the top level function signatures from a list of
-- top level defintions
getSigs :: [TopDef] -> Typecheck Signatures
getSigs topDefs = do
  sigInfo <- mapM getSigInfo topDefs

  -- Check for duplicate function names.
  let funIds = map fst sigInfo
  when (nubOrd funIds /= funIds)
    $ err $ Error "Duplicate top-level function identifier"

  return $ M.fromList sigInfo

  where
    getSigInfo :: TopDef -> Typecheck (Ident, ([Type], Type))
    getSigInfo (FnDef retType id args _) =
      getArgTypes args >>= \ argTypes -> return (id, (argTypes, retType))

    -- Returns the argument types from a list of arguments, fails if
    -- there are issues with any arguments.
    getArgTypes :: [Arg] -> Typecheck [Type]
    getArgTypes args = do
      let (types, ids) = unzip . map getArg $ args

      -- Check for duplicate argument names or void argument types.
      when (nubOrd ids /= ids) $ err $ Error "Duplicate argument identifier"
      when (Void `elem` types) $ err $ Error "Function argument has type Void"

      return types

      where
        getArg :: Arg -> (Type, Ident)
        getArg (Argument typ id) = (typ, id)

-- | Version of (==) that treats a function call with
-- return type T as equal to a `normal` type T.
tEq :: Type -> Type -> Bool
tEq t (Fun retType _) = t `tEq` retType
tEq (Fun retType _) t = retType `tEq` t
tEq t t'              = t == t'

-- | Version of @elem@ that treats a function call with return type T
-- as equal to a `normal type T.
okType :: Type -> [Type] -> Bool
okType _ []       = False
okType t (x : xs) = t `tEq` x || okType t xs

err :: Error -> Typecheck a
err = E.throwError
