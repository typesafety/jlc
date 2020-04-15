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

{-# LANGUAGE LambdaCase #-}

module AlphaRename
  ( alphaRename
  ) where


import           Control.Applicative ((<|>))
import           Data.Bifunctor (first, second, bimap)

import qualified Control.Monad.State as ST
import qualified Data.Map.Strict as M

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
      renameItem (NoInit _id)   = NoInit <$> stepIdent
      renameItem (Init _id expr) = Init <$> stepIdent <*> renameExpr expr

  -- Ass id expr ->

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

-- * Helper functions for manipulating the environment.

-- | Adds an empty context to the top of the environment.
pushCxt :: Rename ()
pushCxt = ST.modify $ first (M.empty :)

{- | Removes the top context from the environment. Crashes if the
environment is empty.
-}
popCxt :: Rename ()
popCxt = ST.modify $ first tail'
  where
    tail' :: [a] -> [a]
    tail' = \case
      []     -> error "popCxt: could not pop context from empty stack"
      x : xs -> xs

-- | Sets the variable counter to the given value.
setCounter :: Int -> Rename ()
setCounter n = ST.modify $ second (const n)

-- | Increment the variable counter by one.
incrCounter :: Rename ()
incrCounter = ST.modify $ second (+ 1)

-- | Get the current value of the variable counter.
getCounter :: Rename Int
getCounter = ST.gets snd

{- | Looks up the alpha-renamed version of an identifier, given the
original name. Crashes if it cannot be found; the typechecking phase
should make failure impossible.
-}
lookupVar :: Ident -> Rename Ident
lookupVar id = ST.gets (find id . fst) >>= \case
  Just renamedVar -> return renamedVar
  Nothing         -> error $ "lookupVar: could not find variable "
                           ++ show id ++ " in environment"
  where
    find :: Ident -> [Context] -> Maybe Ident
    find id = foldl (<|>) Nothing . map (M.lookup (Original id))

nextVar :: Rename String
nextVar = (\ n -> varBase ++ show n) <$> getCounter

nextIdent :: Rename Ident
nextIdent = Ident <$> nextVar

{- | Return the next variable name as an Ident, and increment the
ariable counter to the next free number. This both returns a value
AND modifies the state.
-}
stepIdent :: Rename Ident
stepIdent = nextIdent >>= \ id -> incrCounter >> return id

-- * Other helper functions and constants.

varBase :: String
varBase = "v"
