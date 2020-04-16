{-# LANGUAGE LambdaCase #-}

{- | Module for running alpha-renaming of typechecked ASTs in order

to make code generation easier. Each variable is replaced with
a constant variable name suffixed with a unique counter.

For example:

int f() {
  int x = 0;
  int y = 2
  {
    int x = 5;
  }
  y = x;
  x++;
  return y
}

Converts to:

int f() {
  int v0 = 0;
  int v1 = 2;
  {
    int v2 = 5;
  }
  int v3 = v0
  int v4 = v0 + 1;
  return v3;
}
-}

module AlphaRename
  ( alphaRename
  ) where


import           Control.Applicative ((<|>))
import           Data.Bifunctor (first, second, bimap)

import qualified Control.Monad.State as ST
import qualified Data.Map.Strict as M
import qualified GHC.Stack as Stack

import           Javalette.Abs

newtype Original = Original Ident
  deriving (Eq, Ord)

type Context = M.Map Original Ident

{- | The environment maps the original names to their alpha-renamed names,
and keeps the variable count.
-}
type Env = ([Context], Int)

type Rename a = ST.State Env a

runRename :: Rename a -> a
runRename p = ST.evalState p emptyState
  where
    emptyState = ([], 0)

{- | Takes an annotated AST and returns a syntactically identical
AST where all variable names are unique. That is, any variable assignments
will appear only once per variable name.
-}
alphaRename :: Prog -> Prog
alphaRename = runRename . rename

rename :: Prog -> Rename Prog
rename (Program topDefs) = Program <$> mapM renameDef topDefs

renameDef :: TopDef -> Rename TopDef
renameDef (FnDef typ id args (Block stmts)) = do
  -- For each new function definition, open up a new context
  -- and reset the variable counter.
  setCounter 0
  pushCxt

  newArgs <- mapM renameArg args
  newStmts <- mapM renameStmt stmts

  popCxt

  return $ FnDef typ id newArgs (Block newStmts)

  where
    renameArg :: Arg -> Rename Arg
    renameArg (Argument typ id) = Argument typ <$> stepIdent

renameStmt :: Stmt -> Rename Stmt
renameStmt stmt = case stmt of
  BStmt (Block blkStmts) -> do
    pushCxt
    newBlkStmts <- mapM renameStmt blkStmts
    popCxt
    return $ BStmt (Block newBlkStmts)

  Decl typ items -> Decl typ <$> mapM renameItem items
    where
      renameItem :: Item -> Rename Item
      renameItem (NoInit id) = NoInit <$> bindStepIdent id
      renameItem (Init id expr) = do
        -- We rename expressions FIRST to properly handle cases like
        -- int x = x + 1
        -- where x was previously bound.
        newExpr <- renameExpr expr
        newId <- bindStepIdent id
        return $ Init newId newExpr

  Ass id expr -> do
    -- Again, important to consider the order of evaluation; we
    -- rename the expression FIRST.
    newExpr <- renameExpr expr
    newId <- rebindStepIdent id
    return $ Ass newId newExpr

  -- Incr id ->



  -- Decr Ident
  -- Ret Expr
  -- VRet
  -- If Expr Stmt
  -- IfElse Expr Stmt Stmt
  -- While Expr Stmt
  -- SExp Expr

  _ -> return stmt

renameExpr :: Expr -> Rename Expr
renameExpr = undefined

--
-- * Helper functions for manipulating the environment.
--

{- | Crash with an error message and the call stack if
the context stack is empty. Otherwise, return the context stack.
-}
fetchCxts :: Stack.HasCallStack => Rename [Context]
fetchCxts = ST.gets fst >>= \ cxts -> case cxts of
  [] -> error $ "empty context stack:\n"
              ++ Stack.prettyCallStack Stack.callStack
  _  -> return cxts

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

-- | Like @bindVar@, but automatically uses the next unique variable name.
-- TODO: Consider removing
-- bindNew :: Ident -> Rename ()
-- bindNew orig = nextIdent >>= bindVar (Original orig)

{- | Given an original variable name, look up its alpha-name and replace
the topmost binding in the context stack and rebind it to a new unique
variable. For example:

@
Context stack: [fromList [("x", "v1"), ("y", "v2")], fromList [("x", "v0")]]
>>> rebind "x"

New stack: [fromList [("x", "v3"), ("y", "v2")], fromList [("x", "v0")]]
@
-}
rebind :: Original -> Rename ()
rebind orig = do
  v <- nextIdent
  ST.modify $ first $ applyWhen (M.member orig) (M.adjust (const v) orig)
  where
    -- Apply a function on the first element in the list that
    -- satisfies the predicate.
    applyWhen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
    applyWhen _ _ [] = []
    applyWhen p f (x : xs)
      | p x       = f x : xs
      | otherwise = x : applyWhen p f xs

{- | Looks up the alpha-renamed version of an identifier, given the
original name. Crashes if it cannot be found; the typechecking phase
should make failure impossible, however.
-}
lookupVar :: Ident -> Rename Ident
lookupVar id = ST.gets (find id . fst) >>= \case
  Just renamedVar -> return renamedVar
  Nothing         -> error $ "lookupVar: could not find variable "
                           ++ show id ++ " in environment"
  where
    find :: Ident -> [Context] -> Maybe Ident
    find id = foldl (<|>) Nothing . map (M.lookup (Original id))

-- | Return the next available unique variable name as an Ident.
nextIdent :: Rename Ident
nextIdent = Ident <$> nextVar
  where
    nextVar :: Rename String
    nextVar = (\ n -> varBase ++ show n) <$> getCounter

{- | Return the next variable name as an Ident, and increment the
ariable counter to the next free number. This both returns a value
AND modifies the state.
-}
stepIdent :: Rename Ident
stepIdent = nextIdent >>= \ id -> incrCounter >> return id

{- | Bind the next unique variable name @v@ to the given original id, then
increment the counter and return @v@.

This returns a value, modifies the context stack, and modifies the counter.
-}
bindStepIdent :: Ident -> Rename Ident
bindStepIdent id = do
  v <- nextIdent
  bindVar (Original id) v
  incrCounter
  return v

-- | Like @bindStepIdent@, but using @rebind@ instead of @bindVar@.
rebindStepIdent :: Ident -> Rename Ident
rebindStepIdent id = do
  rebind (Original id)
  incrCounter
  lookupVar id

--
-- * Other helper functions and constants.
--

varBase :: String
varBase = "v"
