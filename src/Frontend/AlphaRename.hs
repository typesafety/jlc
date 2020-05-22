{-# LANGUAGE LambdaCase #-}

{- | Module for running alpha-renaming of ASTs in order
to make code generation easier. Each variable is replaced with
a constant variable name suffixed with a unique counter.

For the sake of clarity, when referring to the original and renamed
variables in the environment; this module should refer to the original
variable names as IDs (id), and the alpha-renamed variants as VARIABLEs
(var)
-}

module Frontend.AlphaRename
       ( alphaRename
       ) where

import Control.Applicative ((<|>))
import Data.Bifunctor (first, second)
import Data.Maybe (fromMaybe)

import Javalette.Abs

import qualified Control.Monad.State.Strict as ST
import qualified Data.Map.Strict as M
import qualified GHC.Stack as Stack


newtype Original = Original Ident
  deriving (Eq, Ord)

type Context = M.Map Original Ident

{- | The environment maps the original names to their alpha-renamed names,
and keeps the variable count.
-}
type Env = ([Context], Int)

type Rename a = ST.State Env a

runRename :: Rename a -> a
runRename p = ST.evalState p newEnv

newEnv :: Env
newEnv = ([M.empty], 0)

{- | Takes an AST and returns an equivalent AST where all variable
names are unique. That is, regardless of scope, any variable names
are used only once per function definition.
-}
alphaRename :: Prog -> Prog
alphaRename = runRename . rename

rename :: Prog -> Rename Prog
rename (Program defs) =
  -- Each top-level function is renamed independently of another.
  Program <$> mapM (ST.withState (const newEnv) . renameDef) defs

renameDef :: TopDef -> Rename TopDef
renameDef (FnDef typ ident args blk) = do
  aArgs <- mapM renameArg args
  aBlk <- renameBlk blk
  return $ FnDef typ ident aArgs aBlk

  where
    renameArg :: Arg -> Rename Arg
    renameArg (Argument t i) = Argument t <$> newBindStepAlpha i

renameBlk :: Blk -> Rename Blk
renameBlk (Block stmts) = Block <$> mapM renameStmt stmts

renameStmt :: Stack.HasCallStack => Stmt -> Rename Stmt
renameStmt = \case
  BStmt blk -> do
    pushCxt
    aBlk <- renameBlk blk
    popCxt
    return $ BStmt aBlk

  Decl typ items -> Decl typ <$> mapM renameItem items
    where
      renameItem :: Item -> Rename Item
      renameItem (NoInit ident) = NoInit <$> newBindStepAlpha ident
      renameItem (Init ident expr) = do
        -- We rename the expression FIRST to properly handle cases like
        -- int x = x + 1
        -- where x was previously bound.
        aExpr <- renameExpr expr
        aVar <- newBindStepAlpha ident
        return $ Init aVar aExpr

  Ass var expr -> do
    -- Again, important to consider the order of evaluation; we
    -- do renaming on the expression FIRST.
    aExpr <- renameExpr expr
    -- Then, look up the a-var of the original id that is being assigned to.
    aVar <- renameVar var
    return $ Ass aVar aExpr

  Incr var -> incrDecr Incr var
  Decr var -> incrDecr Decr var

  Ret expr -> Ret <$> renameExpr expr

  If expr stmt -> If <$> renameExpr expr <*> renameStmt stmt

  IfElse expr s1 s2 ->
    IfElse <$> renameExpr expr <*> renameStmt s1 <*> renameStmt s2

  While expr stmt -> While <$> renameExpr expr <*> renameStmt stmt

  ForEach typ ident expr stmt ->
    ForEach typ
      <$> newBindStepAlpha ident
      <*> renameExpr expr
      <*> renameStmt stmt

  SExp expr -> SExp <$> renameExpr expr

  -- Catch-all case for statements that need no renaming.
  stmt -> return stmt

  where
    incrDecr :: (Var -> Stmt) -> Var -> Rename Stmt
    incrDecr c (IdVar ident)       = c . IdVar <$> lookupVar (Original ident)
    incrDecr c (ArrVar ident idxs) = do
      aIdxs <- mapM renameArrIdx idxs
      aIdent <- lookupVar (Original ident)
      return $ c (ArrVar aIdent aIdxs)

renameArrIdx :: ArrIndex -> Rename ArrIndex
renameArrIdx (ArrIndex e) = ArrIndex <$> renameExpr e

renameVar :: Var -> Rename Var
renameVar (IdVar ident)          = IdVar <$> lookupVar (Original ident)
renameVar (ArrVar ident arrIdxs) = do
  aArrIdxs <- mapM renameArrIdx arrIdxs
  aIdent <- lookupVar (Original ident)
  return $ ArrVar aIdent aArrIdxs

renameExpr :: Expr -> Rename Expr
renameExpr = \case
  ENewArr t expr   -> ENewArr t <$> renameExpr expr
  ELength var      -> ELength <$> renameVar var
  EVar var         -> EVar <$> renameVar var
  EApp ident exprs -> EApp ident <$> mapM renameExpr exprs
  Neg  expr        -> Neg <$> renameExpr expr
  Not  expr        -> Not <$> renameExpr expr
  EMul e1 op e2    -> binOpRename (flip EMul op) e1 e2
  EAdd e1 op e2    -> binOpRename (flip EAdd op) e1 e2
  ERel e1 op e2    -> binOpRename (flip ERel op) e1 e2
  EAnd e1 e2       -> binOpRename EAnd e1 e2
  EOr  e1 e2       -> binOpRename EOr e1 e2
  AnnExp expr typ  -> flip AnnExp typ <$> renameExpr expr

  -- Catch-all for cases which do not need renaming.
  expr -> return expr

  where
    binOpRename :: (Expr -> Expr -> Expr) -> Expr -> Expr -> Rename Expr
    binOpRename constr e1 e2 = constr <$> renameExpr e1 <*> renameExpr e2

--
-- * Helper functions for manipulating the environment.
--

{- | Crash with an error message and the call stack if
the context stack is empty. Otherwise, return the context stack.
-}
fetchCxts :: Stack.HasCallStack => Rename [Context]
fetchCxts = ST.gets fst >>= \case
  []   -> error $ stackStr ++ "\nempty context stack"
  cxts -> return cxts

-- | Adds an empty context to the top of the environment.
pushCxt :: Rename ()
pushCxt = ST.modify $ first (M.empty :)

{- | Removes the top context from the environment. Crashes if the
environment is empty.
-}
popCxt :: Rename ()
popCxt = fetchCxts >> ST.modify (first tail)

-- | Increment the variable counter by one.
incrCounter :: Rename ()
incrCounter = ST.modify $ second (+ 1)

-- | Get the current value of the variable counter.
getCounter :: Rename Int
getCounter = ST.gets snd

{- | Perform some update to the top context in the environment. Crash
if the environment is empty.
-}
updateCxt :: (Context -> Context) -> Rename ()
updateCxt f = fetchCxts >> ST.modify (first $ \ (x : xs) -> f x : xs)

-- | Create a new mapping between an original variable and its alpha-renaming.
bindVar :: Original -> Ident -> Rename ()
bindVar orig ident = updateCxt $ M.insert orig ident

{- | Look up the alpha-variable, given the original id.
Crashes if the original id cannot be found.
-}
lookupVar :: Stack.HasCallStack => Original -> Rename Ident
lookupVar orig = fromMaybe (error errMsg) <$> ST.gets (find orig . fst)
  where
    find :: Original -> [Context] -> Maybe Ident
    find o = foldl (<|>) Nothing . map (M.lookup o)

    errMsg :: Stack.HasCallStack => String
    errMsg = stackStr ++ "\nCould not find original id `" ++ showOrig orig
             ++ "` in environment"

    showOrig :: Original -> String
    showOrig (Original (Ident name)) = name

-- | Return the next available alpha-variable as an Ident.
nextAlpha :: Rename Ident
nextAlpha = Ident <$> nextVar
  where
    nextVar :: Rename String
    nextVar = (\ n -> varBase ++ show n) <$> getCounter

{- | Return the next alpha-variable as an Ident, and increment the
variable counter to the next free number. This both returns a value
AND modifies the state.
-}
stepAlpha :: Rename Ident
stepAlpha = nextAlpha >>= \ ident -> incrCounter >> return ident

{- | Bind the given original id to the next available alpha-variable @v@,
then increment the counter and return @v@.

This returns a value, modifies the context stack, and modifies the counter.
-}
newBindStepAlpha :: Ident -> Rename Ident
newBindStepAlpha = bindStepAlpha bindVar

-- Helper function for newBindStepAlpha and rebindStepAlpha, uses
-- a given function to bind the @id@ parameter to an a-var.
bindStepAlpha :: (Original -> Ident -> Rename ()) -> Ident -> Rename Ident
bindStepAlpha binderFun ident = do
  aVar <- stepAlpha
  binderFun (Original ident) aVar
  return aVar

--
-- * Other helper functions and constants.
--

varBase :: String
varBase = "v"

stackStr :: Stack.HasCallStack => String
stackStr = Stack.prettyCallStack Stack.callStack
