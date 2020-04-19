{-# LANGUAGE LambdaCase #-}

{- | Module for running alpha-renaming of ASTs in order
to make code generation easier. Each variable is replaced with
a constant variable name suffixed with a unique counter.

For the sake of clarity, when referring to the original and renamed
variables in the environment; this module should refer to the original
variable names as IDs (id), and the alpha-renamed variants as VARIABLEs
(var)
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
Crashes if the original id cannot be found; the typechecking phase
should make failure impossible, however.
-}
lookupVar :: Ident -> Rename Ident
lookupVar id = ST.gets (find id . fst) >>= \case
  Just aVar -> return aVar
  Nothing   -> error $ "lookupVar: could not find id "
                     ++ show id ++ " in environment"
  where
    find :: Ident -> [Context] -> Maybe Ident
    find id = foldl (<|>) Nothing . map (M.lookup (Original id))

-- | Return the next available alpha-variable as an Ident.
nextAlpha :: Rename Ident
nextAlpha = Ident <$> nextVar
  where
    nextVar :: Rename String
    nextVar = (\ n -> varBase ++ show n) <$> getCounter

{- | Return the next alpha-variable as an Ident, and increment the
ariable counter to the next free number. This both returns a value
AND modifies the state.
-}
stepAlpha :: Rename Ident
stepAlpha = nextAlpha >>= \ id -> incrCounter >> return id

{- | Bind the given original id to the next available alpha-variable @v@,
then increment the counter and return @v@.

This returns a value, modifies the context stack, and modifies the counter.
-}
bindStepAlpha :: Ident -> Rename Ident
bindStepAlpha id = do
  aVar <- nextAlpha
  bindVar (Original id) aVar
  incrCounter
  return aVar

-- TODO: these two functions are very similar, refactor?
{- | Like @bindStepAlpha@, but using @rebind@ instead of @bindVar@.
In other words, keep the original @id@ on the stac, but replace its
topmost binding to a new unique variable.
-}
rebindStepAlpha :: Ident -> Rename Ident
rebindStepAlpha id = do
  aVar <- nextAlpha
  rebind (Original id) aVar
  incrCounter
  return aVar

--
-- * Other helper functions and constants.
--

varBase :: String
varBase = "v"
