{-# LANGUAGE LambdaCase #-}

module Frontend.TypeChecker
       ( typeCheck
       ) where

import Control.Applicative ((<|>))
import Control.Monad (unless, when, zipWithM)
import Data.Maybe (fromMaybe)

import Frontend.Errors (Error (..))
import Javalette.Abs

import qualified Control.Monad.Except as E
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State as ST
import qualified Data.Map.Strict as M
import qualified GHC.Stack as Stack


--
-- * Type synonyms and related functions
--

type TypeCheck a = R.ReaderT Signatures (ST.StateT Env (E.Except Error)) a

runTypeCheck :: TypeCheck a -> Either Error a
runTypeCheck t =
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

-- | Entry point; typecheck and annotate a parsed program.
typeCheck :: Prog -> Either Error Prog
typeCheck = runTypeCheck . typeCheck'
  where
    typeCheck' :: Prog -> TypeCheck Prog
    typeCheck' (Program topDefs) =
      getSigs topDefs >>= \ sigs -> R.local (M.union sigs) $ do
        checkMain
        annotatedTopDefs <- mapM checkDef topDefs
        return $ Program annotatedTopDefs

--
-- * Functions for typechecking and annotating a program.
--

checkDef :: TopDef -> TypeCheck TopDef
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
  unless (typ == Void || reachableRet stmts) $ throw $ MissingReturnError id

  -- TypeCheck the statements in the function body, annotating expressions.
  annotated <- mapM checkStmt stmts

  -- Finally, when finished, pop the earlier two contexts off the stack
  -- and return the annotated TopDef.
  popCxt
  popCxt

  return $ FnDef typ id args (Block annotated)

-- | Return @True@ if the given list of statements is guaranteed to
-- reach a non-void return statement.
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

-- | TypeCheck a statment and annotate any sub-expressions.
checkStmt :: Stmt -> TypeCheck Stmt
checkStmt = \case
  Empty -> return Empty

  BStmt (Block stmts) -> do
    pushCxt
    checkedStmts <- mapM checkStmt stmts
    popCxt
    return $ BStmt (Block checkedStmts)

  Decl typ items -> do
    checkedItems <- mapM (checkItem typ) items
    return $ Decl typ checkedItems

    where
      checkItem :: Type -> Item -> TypeCheck Item
      checkItem expected item = case item of
        NoInit id -> do
          bindType id expected
          return item
        Init id exp -> do
          annExp <- annotateWithType expected exp
          bindType id expected
          return $ Init id annExp

  Ass var exp -> do
    varType <- lookupVar var
    annExp <- annotateWithType varType exp
    return $ Ass var annExp

  Incr id -> lookupVar id >>= \case
    Int -> return $ Incr id
    typ -> throw $ IncrTypeError id typ

  Decr id -> lookupVar id >>= \case
    Int -> return $ Decr id
    typ -> throw $ DecrTypeError id typ

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

  ForEach typ ident expr stmt -> do
    aExpr <- annotateWithType (Arr typ) expr
    bindType ident typ
    aStmt <- checkStmt stmt
    unbindType ident
    return $ ForEach typ ident aExpr aStmt

  SExp exp -> do
    (annExp, eType) <- annotate2 exp
    if eType `tEq` Void
      then return $ SExp annExp
      else throw $ NonVoidSExpError exp eType

  where
    -- Given a type, assert that the return type of the current
    -- function has the same type, or throw an error if not.
    checkRet :: Type -> TypeCheck ()
    checkRet t = do
      (id, retType) <- getRet
      if t `tEq` retType
        then return ()
        else throw $ ReturnError id retType t

-- | Check for the existence of main() and ensure that it has the
-- correct arguments and return type.
checkMain :: TypeCheck ()
checkMain = R.reader (M.lookup (Ident "main")) >>= \case
  Nothing -> throw $ MainError "Missing main() function"

  Just (argTypes, retType)
    | retType /= Int ->
      throw $ MainError "main() does not not have return type `int`"

    | not $ null argTypes ->
      throw $ MainError "main() must have zero parameters"

    | otherwise -> return ()

-- | Run @annotate@ but throw an error if the inferred type does not
-- match the given type.
annotateWithType :: Stack.HasCallStack => Type -> Expr -> TypeCheck Expr
annotateWithType expected exp = annotate exp >>= \case
  annExp@(AnnExp e t)
    | expected `tEq` t -> return annExp
    | otherwise        -> throw $ ExpError exp [expected] t
  _ -> error "annotateWithType: `annotate` did not return an AnnExp expression"

-- | Like @annotate@, but also return the type of the expression.
annotate2 :: Stack.HasCallStack => Expr -> TypeCheck (Expr, Type)
annotate2 exp = annotate exp >>= \case
  AnnExp e t -> return (e, t)
  _ -> error "annotate2: `annotate` did not return an AnnExp expression"

-- | Given an expression, infer its type and return its annotated version.
-- This also annotates any sub-expressions.
annotate :: Expr -> TypeCheck Expr
annotate topExp = do
  (exp', eType) <- ann
  return $ AnnExp exp' eType

  where
    ann :: Stack.HasCallStack => TypeCheck (Expr, Type)
    ann = case topExp of
      ELitInt _    -> return (topExp, Int)
      ELitDouble _ -> return (topExp, Double)
      ELitTrue     -> return (topExp, Bool)
      ELitFalse    -> return (topExp, Bool)
      EString _    -> return (topExp, Str)

      ENewArr t expr -> do
        annExpr <- annotateWithType Int expr
        let eType = Arr t
        return (ENewArr t annExpr, eType)

      ELength var -> lookupVar var >>= \case
        Arr t -> return (topExp, Int)
        t     -> throw $ NonArrayError topExp t

      EApp id exps -> R.reader (M.lookup id) >>= \case
        Nothing -> throw $ SymbolError id
        Just (argTypes, retType) -> do
          -- Throw an error if number of arguments do not match the
          -- number of parameters to the function.
          when (length argTypes /= length exps)
            $ throw $ NumArgsError id (length argTypes) (length exps)

          annExps <- zipWithM annotateWithType argTypes exps

          return (EApp id annExps, Fun retType argTypes)

      -- Does not support multi-dimensional arrays.
      EVar var@ArrVar{} -> lookupVar var >>= \case
        Arr t -> return (topExp, t)
        t -> error $ "ann: ArrVar was not of type Arr, but rather: " ++ show t

      EVar var@IdVar{} -> do
        typ <- lookupVar var
        return (topExp, typ)

      Neg exp -> do
        (annExp, eType) <- annotate2 exp
        let allowed = [Int, Double]
        if eType `okType` allowed
          then return (Neg annExp, eType)
          else throw $ ExpError exp allowed eType

      Not exp -> do
        (annExp, eType) <- annotate2 exp
        if eType `okType` [Bool]
          then return (Not annExp, eType)
          else throw $ ExpError exp [Bool] eType

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
        -- Check and annotate binary operations. Return the annotated
        -- expressions and their types. Assumes that type casting is
        -- forbidden, and thus, that the two expressions have the same type.
        annBinOp :: [Type]      -- Allowed types for operands
                 -> Maybe Type  -- Optional enforced return type
                 -> Expr        -- Left operand
                 -> Expr        -- Right operand
                 -> TypeCheck (Expr, Expr, Type)
        annBinOp allowedTypes mbyRetType e1 e2 = do
          (annE1, type1) <- annotate2 e1
          (annE2, type2) <- annotate2 e2
          if type1 `okType` allowedTypes && type1 `tEq` type2
            then return (annE1, annE2, fromMaybe type1 mbyRetType)
            else throw $ ExpError e2 allowedTypes type2

--
-- * Functions related to modifying the environment.
--

bindArgs :: Stack.HasCallStack => [Arg] -> TypeCheck ()
bindArgs args = ST.get >>= \case
  []     -> error "bindArgs: empty context stack"
  c : cs -> do
    let c' = M.union c . M.fromList . map argToTuple $ args
    ST.put $ c' : cs

  where
    argToTuple :: Arg -> (Ident, Type)
    argToTuple (Argument typ id) = (id, typ)

bindType :: Stack.HasCallStack => Ident -> Type -> TypeCheck ()
bindType id typ = ST.get >>= \case
  []    -> error "bindType: empty context stack"
  c : _ -> case M.lookup id c of
    Just _  -> throw $ DuplicateDeclError id
    Nothing -> updateCxt (M.insert id typ)

unbindType :: Stack.HasCallStack => Ident -> TypeCheck ()
unbindType ident = updateCxt $ \ cxt ->
  if ident `M.member`cxt
    then M.delete ident cxt
    else error "unbindType: ident not found in top context"

updateCxt :: Stack.HasCallStack => (Context -> Context) -> TypeCheck ()
updateCxt f = ST.get >>= \case
  []     -> error "updateCxt: empty context stack"
  c : cs -> ST.put $ f c : cs

pushCxt :: TypeCheck ()
pushCxt = ST.modify (M.empty :)

popCxt :: Stack.HasCallStack => TypeCheck ()
popCxt = ST.get >>= \case
  []     -> error "popCxt: empty context stack"
  _ : cs -> ST.put cs

--
-- * Various helper functions.
--

-- | Return the current function's identifier and return type.
-- Assumes that the context stack is implemented such that the bottom
-- context contains only a single binding (the one of the current function).
getRet :: TypeCheck (Ident, Type)
getRet = ST.get >>= \ env -> case getRet' env of
  Left msg      -> error msg
  Right binding -> return binding

  where
    getRet' :: Env -> Either String (Ident, Type)
    getRet' []  = Left "getRet: empty context stack"
    getRet' env = case M.toList . last $ env of
      []    -> Left "getRet: no binding in bottom context"
      c : _ -> Right c

-- | Given an identifier, check if it exists in the current context stack.
-- If so, return the type of its topmost ("latest") occurrence, or throw an
-- error otherwise.
lookupVar :: Var -> TypeCheck Type
lookupVar (ArrVar id _) = lookupVar (IdVar id)
lookupVar (IdVar id)    = ST.get >>= \ cxts -> case lookupVar' id cxts of
  Just typ -> return typ
  Nothing  -> throw $ SymbolError id
  where
    lookupVar' :: Ident -> Env -> Maybe Type
    lookupVar' _ []        = Nothing
    lookupVar' id (c : cs) = M.lookup id c <|> lookupVar' id cs

-- | Get the top level function signatures from a list of
-- top level definitions.
getSigs :: [TopDef] -> TypeCheck Signatures
getSigs topDefs = do
  sigInfo <- mapM getSigInfo topDefs

  -- Check for duplicate function names.
  let dupeIds = dupes . map fst $ sigInfo
  unless (null dupeIds) $ throw $ DuplicateFunError (head dupeIds)

  return $ M.fromList sigInfo

  where
    -- Return the list of elements that have duplicates in the input list.
    dupes :: Eq a => [a] -> [a]
    dupes [] = []
    dupes (x : xs)
      | x `elem` xs = x : filter (/= x) (dupes xs)
      | otherwise   = dupes xs

    getSigInfo :: TopDef -> TypeCheck (Ident, ([Type], Type))
    getSigInfo (FnDef retType id args _) =
      getArgTypes args >>= \ argTypes -> return (id, (argTypes, retType))

      where
        -- Return the argument types from a list of arguments, fail if
        -- there are issues with any arguments.
        getArgTypes :: [Arg] -> TypeCheck [Type]
        getArgTypes args = do
          let getArg (Argument t i) = (t, i)
              (types, ids) = unzip . map getArg $ args

          -- Check for duplicate argument names or void argument types.
          unless (null . dupes $ ids) $ throw $ DuplicateParamError id
          when (Void `elem` types) $ throw $ VoidParamError id

          return types

-- | Version of @(==)@ that treats a function call with
-- _return type_ T as equal to the _actual_ type T.
tEq :: Type -> Type -> Bool
tEq t (Fun retType _) = t `tEq` retType
tEq (Fun retType _) t = retType `tEq` t
tEq t t'              = t == t'

-- | Version of @elem@ that treats a function call with
-- _return type_ T as equal to the _actual_ type T.
okType :: Type -> [Type] -> Bool
okType _ []       = False
okType t (x : xs) = t `tEq` x || okType t xs

throw :: Error -> TypeCheck a
throw = E.throwError
