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
  E.runExcept $ ST.evalStateT (R.runReaderT t initSig) emptyEnv

type Signatures = M.Map Ident ([Type], Type)

type Context = M.Map Ident Type

type Env = [Context]

initSig :: Signatures
initSig = M.fromList
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
checkDef (FnDef typ ident args (Block stmts)) = do
  -- First, add the function return type as the only binding in
  -- the bottom context of the stack. This bottom context will contain
  -- only this binding and serves only to store the return type.
  pushCxt
  bindType ident typ

  -- Then, push another context onto the stack and bind the types
  -- of the function arguments.
  pushCxt
  bindArgs args

  -- Ensure reachable return statement(s) for non-void functions.
  -- It is enough to check that return statements exists and are
  -- reachable; any type-incorrect returnts will be caught in the
  -- later call to `checkStmts`.
  unless (typ == Void || reachableRet stmts) $ throw $ MissingReturnError ident

  -- TypeCheck the statements in the function body, annotating expressions.
  annotated <- mapM checkStmt stmts

  -- Finally, when finished, pop the earlier two contexts off the stack
  -- and return the annotated TopDef.
  popCxt
  popCxt

  return $ FnDef typ ident args (Block annotated)

-- | Return @True@ if the given list of statements is guaranteed to
-- reach a non-void return statement.
reachableRet :: [Stmt] -> Bool
reachableRet []       = False
reachableRet (s : ss) = case s of
  Ret _                 -> True
  BStmt (Block stmts)   -> reachableRet stmts  || reachableRet ss
  If ELitTrue stmt      -> reachableRet [stmt] || reachableRet ss
  While ELitTrue stmt   -> reachableRet [stmt] || reachableRet ss
  IfElse expr s1 s2
    | expr == ELitTrue  -> reachableRet [s1] || reachableRet ss
    | expr == ELitFalse -> reachableRet [s2] || reachableRet ss
    | otherwise         -> (reachableRet [s1] && reachableRet [s2])
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
        NoInit ident -> do
          bindType ident expected
          return item
        Init ident expr -> do
          annExpr <- annotateWithType expected expr
          bindType ident expected
          return $ Init ident annExpr

  Ass var expr -> do
    varType <- lookupVar var
    annExpr <- annotateWithType varType expr
    return $ Ass var annExpr

  Incr ident -> lookupVar ident >>= \case
    Int -> return $ Incr ident
    typ -> throw $ IncrTypeError ident typ

  Decr ident -> lookupVar ident >>= \case
    Int -> return $ Decr ident
    typ -> throw $ DecrTypeError ident typ

  Ret expr -> do
    (annExpr, typ) <- annotate2 expr
    checkRet typ
    return $ Ret annExpr

  VRet -> do
    checkRet Void
    return VRet

  If expr stmt -> do
    annExpr <- annotateWithType Bool expr
    checkedStmt <- checkStmt stmt
    return $ If annExpr checkedStmt

  IfElse expr s1 s2 -> do
    annExpr <- annotateWithType Bool expr
    checkedS1 <- checkStmt s1
    checkedS2 <- checkStmt s2
    return $ IfElse annExpr checkedS1 checkedS2

  While expr stmt -> do
    annExpr <- annotateWithType Bool expr
    checkedStmt <- checkStmt stmt
    return $ While annExpr checkedStmt

  ForEach typ ident expr stmt -> do
    aExpr <- annotateWithType (Arr typ) expr
    bindType ident typ
    aStmt <- checkStmt stmt
    unbindType ident
    return $ ForEach typ ident aExpr aStmt

  SExp expr -> do
    (annExpr, eType) <- annotate2 expr
    if eType `tEq` Void
      then return $ SExp annExpr
      else throw $ NonVoidSExpError expr eType

  where
    -- Given a type, assert that the return type of the current
    -- function has the same type, or throw an error if not.
    checkRet :: Type -> TypeCheck ()
    checkRet t = do
      (ident, retType) <- getRet
      if t `tEq` retType
        then return ()
        else throw $ ReturnError ident retType t

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
annotateWithType expected expr = annotate expr >>= \case
  annExpr@(AnnExp _ t)
    | expected `tEq` t -> return annExpr
    | otherwise        -> throw $ ExpError expr [expected] t
  _ -> error "annotateWithType: `annotate` did not return an AnnExp expression"

-- | Like @annotate@, but also return the type of the expression.
annotate2 :: Stack.HasCallStack => Expr -> TypeCheck (Expr, Type)
annotate2 expr = annotate expr >>= \case
  AnnExp e t -> return (e, t)
  _ -> error "annotate2: `annotate` did not return an AnnExp expression"

-- | Given an expression, infer its type and return its annotated version.
-- This also annotates any sub-expressions.
annotate :: Expr -> TypeCheck Expr
annotate topExpr = do
  (expr', eType) <- ann
  return $ AnnExp expr' eType

  where
    ann :: Stack.HasCallStack => TypeCheck (Expr, Type)
    ann = case topExpr of
      ELitInt _    -> return (topExpr, Int)
      ELitDouble _ -> return (topExpr, Double)
      ELitTrue     -> return (topExpr, Bool)
      ELitFalse    -> return (topExpr, Bool)
      EString _    -> return (topExpr, Str)

      ENewArr t expr -> do
        annExpr <- annotateWithType Int expr
        let eType = Arr t
        return (ENewArr t annExpr, eType)

      ELength var -> lookupVar var >>= \case
        Arr _ -> return (topExpr, Int)
        t     -> throw $ NonArrayError topExpr t

      EApp ident exprs -> R.reader (M.lookup ident) >>= \case
        Nothing -> throw $ SymbolError ident
        Just (argTypes, retType) -> do
          -- Throw an error if number of arguments do not match the
          -- number of parameters to the function.
          when (length argTypes /= length exprs)
            $ throw $ NumArgsError ident (length argTypes) (length exprs)

          annExprs <- zipWithM annotateWithType argTypes exprs

          return (EApp ident annExprs, Fun retType argTypes)

      -- Does not support multi-dimensional arrays.
      EVar var -> do
        typ <- lookupVar var
        return (topExpr, typ)

      Neg expr -> do
        (annExpr, eType) <- annotate2 expr
        let allowed = [Int, Double]
        if eType `okType` allowed
          then return (Neg annExpr, eType)
          else throw $ ExpError expr allowed eType

      Not expr -> do
        (annExpr, eType) <- annotate2 expr
        if eType `okType` [Bool]
          then return (Not annExpr, eType)
          else throw $ ExpError expr [Bool] eType

      EMul e1 op e2 -> do
        (annE1, annE2, eType) <- annBinOp allowedOperands Nothing e1 e2
        return (EMul annE1 op annE2, eType)
        where
          allowedOperands :: [Type]
          allowedOperands
            | op `elem` [Times, Div] = [Int, Double]
            | op == Mod              = [Int]
            | otherwise = error $ "EMul: unexpected operand" ++ show op

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
            | otherwise = error $ "ERel: unexpected operand" ++ show op

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
    argToTuple (Argument typ ident) = (ident, typ)

bindType :: Stack.HasCallStack => Ident -> Type -> TypeCheck ()
bindType ident typ = ST.get >>= \case
  []    -> error "bindType: empty context stack"
  c : _ -> case M.lookup ident c of
    Just _  -> throw $ DuplicateDeclError ident
    Nothing -> updateCxt (M.insert ident typ)

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
pushCxt = ST.modify (emptyCxt :)

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

lookupVar (ArrVar ident idxs) =
  typeAtDepth (length idxs) <$> lookupVar (IdVar ident)
  where
    typeAtDepth :: Stack.HasCallStack => Int -> Type -> Type
    typeAtDepth 0 t       = t
    typeAtDepth n (Arr t) = typeAtDepth (n - 1) t
    typeAtDepth _ _ = error $ "typeAtDepth: unhandled mismatch in indexing of"
                            ++ "array variable: " ++ show (ArrVar ident idxs)

lookupVar (IdVar ident) = ST.get >>= \ cxts -> case lookupVar' ident cxts of
  Just typ -> return typ
  Nothing  -> throw $ SymbolError ident
  where
    lookupVar' :: Ident -> Env -> Maybe Type
    lookupVar' _ []        = Nothing
    lookupVar' i (c : cs) = M.lookup i c <|> lookupVar' i cs

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
    getSigInfo (FnDef retType ident args _) =
      getArgTypes args >>= \ argTypes -> return (ident, (argTypes, retType))

      where
        -- Return the argument types from a list of arguments, fail if
        -- there are issues with any arguments.
        getArgTypes :: [Arg] -> TypeCheck [Type]
        getArgTypes as = do
          let getArg :: Arg -> (Type, Ident)
              getArg (Argument t i) = (t, i)
          let (types, ids) = unzip . map getArg $ as

          -- Check for duplicate argument names or void argument types.
          unless (null . dupes $ ids) $ throw $ DuplicateParamError ident
          when   (Void `elem` types)  $ throw $ VoidParamError ident

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
