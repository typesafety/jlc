module Typechecker where

import Debug.Trace

import           Control.Monad (when)
import qualified Control.Monad.Except as E
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State as ST
import           Data.Containers.ListUtils (nubOrd)
import qualified Data.Map.Strict as M

import Javalette.Abs

import Errors (Error (..))

type Typecheck a = R.ReaderT Env (ST.StateT Signatures (E.Except Error)) a

runTypecheck :: Typecheck a -> Either Error a
runTypecheck t =
  E.runExcept $ ST.evalStateT (R.runReaderT t emptyEnv) emptySig

type Signatures = M.Map Ident ([Type], Type)

emptySig :: Signatures
emptySig = M.empty

type Context = M.Map Ident Type

emptyCxt :: Context
emptyCxt = M.empty

type Env = [Context]

emptyEnv :: Env
emptyEnv = []

-- | Typecheck a parsed program.
typecheck :: Prog -> Typecheck Prog
typecheck (Program topdefs) = do
  undefined
  -- sigs <- getSigs topdefs

  -- checkMain sigs

  -- trace (show sigs) $ return ()
  -- return undefined



-- -- | Checks for the existence of main(), and that it has the
-- -- correct arguments and return type.
-- checkMain :: Signatures -> Err ()
-- checkMain sigs = case M.lookup (Ident "main") sigs of
--   Nothing -> fail "Missing main() function"
--   Just (argTypes, retType)
--     | retType /= Int      -> fail "main() does not not have return type `int`"
--     | not $ null argTypes -> fail "main() must have zero arguments"
--     | otherwise           -> return ()

-- -- | Get the top level function signatures from a list of
-- -- top level defintions
-- getSigs :: [TopDef] -> Err Signatures
-- getSigs topDefs = do
--   sigInfo <- mapM getSigInfo topDefs

--   -- Check for duplicate function names.
--   let funIds = map fst sigInfo
--   when (nubOrd funIds /= funIds)
--     $ fail "Duplicate top-level function identifier"

--   return $ M.fromList sigInfo

-- getSigInfo :: TopDef -> Err (Ident, ([Type], Type))
-- getSigInfo (FnDef retType id args _) =
--   getArgTypes args >>= \ argTypes -> return (id, (argTypes, retType))

-- -- | Returns the argument types from a list of arguments, fails if
-- -- there are issues with any arguments.
-- getArgTypes :: [Arg] -> Err [Type]
-- getArgTypes args = do
--   let (types, ids) = unzip . map getArg $ args

--   -- Check for duplicate argument names or void argument types.
--   when (nubOrd ids /= ids) $ fail "Duplicate argument identifier"
--   when (Void `elem` types) $ fail "Function argument has type Void"

--   return types

--   where
--     getArg :: Arg -> (Type, Ident)
--     getArg (Argument typ id) = (typ, id)



