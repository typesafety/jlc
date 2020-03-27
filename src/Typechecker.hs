{-# LANGUAGE LambdaCase #-}

module Typechecker where

import Debug.Trace

import           Control.Applicative ((<|>))
import           Control.Monad (when)
import qualified Control.Monad.Except as E
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State as ST
import           Data.Containers.ListUtils (nubOrd)
import qualified Data.Map.Strict as M

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
  , (Ident "printString", ([],       Void))
  ]

emptyCxt :: Context
emptyCxt = M.empty

emptyEnv :: Env
emptyEnv = []

-- | Typecheck (and annotate?) a parsed program.
typecheck :: Prog -> Typecheck Prog
typecheck prog@(Program topdefs) = do
  sigs <- getSigs topdefs

  trace (show topdefs) $ return ()

  checkMain sigs
  annotated <- checkDefs prog

  trace (show annotated) $ return ()
  return prog

-- * Functions for typechecking and annotating a program.

checkDefs :: Prog -> Typecheck Prog
checkDefs (Program topDefs) = Program <$> mapM checkDef topDefs

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

  -- Typecheck the statements in the function body.
  annotated <- checkStmts stmts

  -- Finally, when finished, pop the earlier two contexts off the stack
  -- and return the annotated TopDef.
  popCxt
  popCxt

  return $ FnDef typ id args (Block annotated)

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
    return $ BStmt $ Block checkedStmts

  Decl typ items -> do
    checkedItems <- mapM (checkItem typ) items
    return $ Decl typ checkedItems

  Ass id exp -> do
    varType <- lookupVar id
    annExp <- checkExp [varType] exp
    bindType id varType
    return $ Ass id annExp

  Incr id@(Ident name) -> lookupVar id >>= \case
    Int -> return $ Incr id
    typ -> err $ Error $ "Incrementing " ++ name ++ " requires type"
                       ++ "int, but instead got type" ++ show typ

  Decr id@(Ident name) -> lookupVar id >>= \case
    Int -> return $ Incr id
    typ -> err $ Error $ "Decrementing " ++ name ++ " requires type"
                       ++ "int, but instead got type" ++ show typ

  Ret exp -> inferExp exp >>= \case
    annExp@(AnnExp e eType) -> do
      checkRet eType
      return $ Ret annExp
    _ -> error $ "checkStmt:\n"
               ++ "case Ret: inferExp did not return an annotated expression"

  VRet -> do
    checkRet Void
    return VRet

  If exp stmt -> do
    annExp <- checkExp [Bool] exp
    checkedStmt <- checkStmt stmt
    return $ If annExp checkedStmt

  IfElse exp s1 s2 -> do
    annExp <- checkExp [Bool] exp
    checkedS1 <- checkStmt s1
    checkedS2 <- checkStmt s2
    return $ IfElse annExp checkedS1 checkedS2

  While exp stmt -> do
    annExp <- checkExp [Bool] exp
    checkedStmt <- checkStmt stmt
    return $ While annExp checkedStmt

  SExp exp -> inferExp exp >>= \ annExp -> return $ SExp annExp

  where
    -- Given a type, check that the return type of the current
    -- function has the same type, and throw an error if not.
    checkRet :: Type -> Typecheck ()
    checkRet t = do
      (id, retType) <- getRet
      if t == retType
        then return ()
        else err $ ReturnError id retType t

    -- Check that an expression has one the expected types, and if so,
    -- return the annotated expression.
    checkExp :: [Type] -> Expr -> Typecheck Expr
    checkExp okTypes exp = inferExp exp >>= \case
      annExp@(AnnExp e eType) ->
        if eType `elem` okTypes
          then return annExp
          else err $ ExpError exp okTypes eType
      _ -> error "checkExp: inferExp did not return an annotated expression"

checkItem :: Type -> Item -> Typecheck Item
checkItem typ item = case item of
  NoInit id   -> return item
  Init id exp -> inferExp exp >>= \case
    annExp@(AnnExp e eType) ->
      if typ == eType
        then return $ Init id annExp
        else err $ ExpError exp [typ] eType
    _ -> error "checkItem: inferExp did not return an annotated expression"

-- | Checks for the existence of main(), and that it has the
-- correct arguments and return type.
checkMain :: Signatures -> Typecheck ()
checkMain sigs = case M.lookup (Ident "main") sigs of
  Nothing -> err $ Error "Missing main() function"

  Just (argTypes, retType)
    | retType /= Int ->
      err $ Error "main() does not not have return type `int`"

    | not $ null argTypes ->
      err $ Error "main() must have zero arguments"

    | otherwise -> return ()

-- | Given an expression, infer its type and return the annotated expression.
inferExp :: Expr -> Typecheck Expr
inferExp = undefined

-- * Functions related to modifying the context.

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
bindType id typ = updateCxt (M.insert id typ)

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

-- * Various helper functions.

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

getSigInfo :: TopDef -> Typecheck (Ident, ([Type], Type))
getSigInfo (FnDef retType id args _) =
  getArgTypes args >>= \ argTypes -> return (id, (argTypes, retType))

-- | Returns the argument types from a list of arguments, fails if
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

err :: Error -> Typecheck a
err = E.throwError
