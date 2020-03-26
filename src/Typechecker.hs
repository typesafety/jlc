module Typechecker where

import Debug.Trace

import Control.Monad (when)
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Map.Strict as M

import Javalette.Abs
import Javalette.ErrM (Err (Ok, Bad))


-- | A Sig is a mapping from function names to their
-- parameter types and return type.
type Signatures = M.Map Ident ([Type], Type)

-- | A Context maps variable names to their types.
type Context = M.Map Ident Type

-- | The environment keeps track of contexts and the top level
-- function signatures.
data Env = Env
  { envSigs :: Signatures
  , envCxts :: [Context]
  }

-- | Typecheck a parsed program.
typecheck :: Prog -> Err Prog
typecheck (Program topdefs) = do
  sigs <- getSigs topdefs

  when (Ident "main" `notElem` M.keys sigs) $ fail "Missing main function"

  trace (show sigs) $ return ()

  return undefined

-- | Get the top level function signatures from a list of
-- top level defintions
getSigs :: [TopDef] -> Err Signatures
getSigs topDefs = do
  sigInfo <- mapM getSigInfo topDefs

  -- Check for duplicate function names.
  let funIds = map fst sigInfo
  when (nubOrd funIds /= funIds)
    $ fail "Duplicate top-level function identifier"

  return $ M.fromList sigInfo

getSigInfo :: TopDef -> Err (Ident, ([Type], Type))
getSigInfo (FnDef retType id args _) =
  getArgTypes args >>= \ argTypes -> return (id, (argTypes, retType))

-- | Returns the argument types from a list of arguments, fails if
-- there are issues with any arguments.
getArgTypes :: [Arg] -> Err [Type]
getArgTypes args = do
  let (types, ids) = unzip . map getArg $ args

  -- Check for duplicate argument names or void argument types.
  when (nubOrd ids /= ids) $ fail "Duplicate argument identifier"
  when (Void `elem` types) $ fail "Function argument has type Void"

  return types

  where
    getArg :: Arg -> (Type, Ident)
    getArg (Argument typ id) = (typ, id)



