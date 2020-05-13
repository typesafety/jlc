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
import Data.Bifunctor (first, second, bimap)
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
renameDef (FnDef typ id args blk) = do
  aArgs <- mapM renameArg args
  aBlk <- renameBlk blk
  return $ FnDef typ id aArgs aBlk

  where
    renameArg :: Arg -> Rename Arg
    renameArg (Argument typ id) = Argument typ <$> newBindStepAlpha id

renameBlk :: Blk -> Rename Blk
renameBlk (Block stmts) = Block <$> mapM renameStmt stmts

renameStmt :: Stmt -> Rename Stmt
renameStmt = \case
  BStmt blk -> do
    pushCxt
    aBlk <- renameBlk blk
    popCxt
    return $ BStmt aBlk

  Decl typ items -> Decl typ <$> mapM renameItem items
    where
      renameItem :: Item -> Rename Item
      renameItem (NoInit id) = NoInit <$> newBindStepAlpha id
      renameItem (Init id expr) = do
        -- We rename the expression FIRST to properly handle cases like
        -- int x = x + 1
        -- where x was previously bound.
        aExpr <- renameExpr expr
        aVar <- newBindStepAlpha id
        return $ Init aVar aExpr

  Ass (ArrVar ident arrIdxs) expr -> do
    aExpr <- renameExpr expr
    aArrIdxs <- mapM renameArrIdx arrIdxs
    aIdent <- lookupVar (Original ident)
    return $ Ass (ArrVar aIdent aArrIdxs) aExpr

  Ass (IdVar ident) expr -> do
    -- Again, important to consider the order of evaluation; we
    -- do renaming on the expression FIRST.
    aExpr <- renameExpr expr
    -- Then, look up the a-var of the original id that is being assigned to.
    aIdent <- lookupVar (Original ident)
    return $ Ass (IdVar aIdent) aExpr

  Incr id -> Incr <$> lookupVar (Original id)
  Decr id -> Decr <$> lookupVar (Original id)

  Ret expr -> Ret <$> renameExpr expr

  If expr stmt -> do
    -- TODO: Check if it's possible to replace with If <$> aExpr <*> aStmt
    aExpr <- renameExpr expr
    aStmt <- renameStmt stmt
    return $ If aExpr aStmt

  IfElse expr s1 s2 -> do
    aExpr <- renameExpr expr
    aS1 <- renameStmt s1
    aS2 <- renameStmt s2
    return $ IfElse aExpr aS1 aS2

  While expr stmt -> do
    aExpr <- renameExpr expr
    aStmt <- renameStmt stmt
    return $ While aExpr aStmt

  SExp expr -> SExp <$> renameExpr expr

  -- Catch-all case for statements that need no renaming.
  stmt -> return stmt

renameArrIdx :: ArrIndex -> Rename ArrIndex
renameArrIdx (ArrIndex e) = ArrIndex <$> renameExpr e

renameExpr :: Expr -> Rename Expr
renameExpr = \case
  EVar id -> EVar <$> lookupVar (Original id)
  EApp id exprs -> EApp id <$> mapM renameExpr exprs
  Neg  expr -> Neg <$> renameExpr expr
  Not  expr -> Not <$> renameExpr expr
  EMul e1 op e2 -> binOpRename (flip EMul op) e1 e2
  EAdd e1 op e2 -> binOpRename (flip EAdd op) e1 e2
  ERel e1 op e2 -> binOpRename (flip ERel op) e1 e2
  EAnd e1 e2    -> binOpRename EAnd e1 e2
  EOr  e1 e2    -> binOpRename EOr e1 e2
  AnnExp expr typ -> flip AnnExp typ <$> renameExpr expr

  -- Catch-all for cases which do not need renaming.
  expr -> return expr

  where
    binOpRename :: (Expr -> Expr -> Expr) -> Expr -> Expr -> Rename Expr
    binOpRename constr e1 e2 = constr <$> renameExpr e1 <*> renameExpr e2

    annExpErr :: String
    annExpErr =
      "AlphaRename.renameExpr: encountered AnnExp; alpha-renaming should be"
      ++ " performed before type checking (and type annotation)"

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

-- | Sets the variable counter to the given value.
setCounter :: Int -> Rename ()
setCounter n = ST.modify $ second (const n)

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
bindVar orig id = updateCxt $ M.insert orig id

{- | Given an original @id@ and a alpha-variable @v@, look up
the topmost binding for @id@ in the context stack and replace its
binding to @v@.

@
Context stack: [fromList [("x", "v1"), ("y", "v2")], fromList [("x", "v0")]]
>>> rebind "x" "v3"

New stack: [fromList [("x", "v3"), ("y", "v2")], fromList [("x", "v0")]]
@
-}
rebind :: Original -> Ident -> Rename ()
rebind orig aVar =
  ST.modify $ first $ applyWhen (M.member orig) (M.adjust (const aVar) orig)
  where
    -- Apply a function on the first element in the list that
    -- satisfies the predicate.
    applyWhen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
    applyWhen _ _ [] = []
    applyWhen p f (x : xs)
      | p x       = f x : xs
      | otherwise = x : applyWhen p f xs

{- | Look up the alpha-variable, given the original id.
Crashes if the original id cannot be found.
-}
lookupVar :: Stack.HasCallStack => Original -> Rename Ident
lookupVar orig = fromMaybe (error errMsg) <$> ST.gets (find orig . fst)
  where
    find :: Original -> [Context] -> Maybe Ident
    find orig = foldl (<|>) Nothing . map (M.lookup orig)

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
stepAlpha = nextAlpha >>= \ id -> incrCounter >> return id

{- | Bind the given original id to the next available alpha-variable @v@,
then increment the counter and return @v@.

This returns a value, modifies the context stack, and modifies the counter.
-}
newBindStepAlpha :: Ident -> Rename Ident
newBindStepAlpha = bindStepAlpha bindVar

{- | Like @bindStepAlpha@, but using @rebind@ instead of @bindVar@.
In other words, keep the original @id@ on the stac, but replace its
topmost binding to a new unique variable.
-}
rebindStepAlpha :: Ident -> Rename Ident
rebindStepAlpha = bindStepAlpha rebind

-- Helper function for newBindStepAlpha and rebindStepAlpha, uses
-- a given function to bind the @id@ parameter to an a-var.
bindStepAlpha :: (Original -> Ident -> Rename a) -> Ident -> Rename Ident
bindStepAlpha binderFun id = do
  aVar <- stepAlpha
  binderFun (Original id) aVar
  return aVar

--
-- * Other helper functions and constants.
--

varBase :: String
varBase = "v"

stackStr :: Stack.HasCallStack => String
stackStr = Stack.prettyCallStack Stack.callStack

idFromVar :: Var -> Ident
idFromVar (IdVar ident)    = ident
idFromVar (ArrVar ident _) = ident
