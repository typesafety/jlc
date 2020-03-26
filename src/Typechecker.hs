{-# LANGUAGE LambdaCase #-}

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

type Typecheck a = R.ReaderT Signatures (ST.StateT Env (E.Except Error)) a

runTypecheck :: Typecheck a -> Either Error a
runTypecheck t =
  E.runExcept $ ST.evalStateT (R.runReaderT t emptySig) emptyEnv

type Signatures = M.Map Ident ([Type], Type)

emptySig :: Signatures
emptySig = M.fromList
  [ (Ident "readInt",     ([],       Int))
  , (Ident "readDouble",  ([],       Double))
  , (Ident "printInt",    ([Int],    Void))
  , (Ident "printDouble", ([Double], Void))
  , (Ident "printString", ([],       Void))
  ]

type Context = M.Map Ident Type

emptyCxt :: Context
emptyCxt = M.empty

type Env = [Context]

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
  return annotated

checkDefs :: Prog -> Typecheck Prog
checkDefs (Program topDefs) = Program <$> mapM checkDef topDefs

checkDef :: TopDef -> Typecheck TopDef
checkDef (FnDef typ id args blk) = undefined

bindType :: Ident -> Type -> Typecheck ()
bindType id typ = updateCxt (M.insert id typ)

updateCxt :: (Context -> Context) -> Typecheck ()
updateCxt f = ST.modify (\ (x : xs) -> f x : xs)

pushCxt :: Typecheck ()
pushCxt = ST.modify (M.empty :)

popCxt :: Typecheck ()
popCxt = ST.get >>= \case
  []       -> error "attempted to pop context from empty stack"
  (x : xs) -> ST.put xs

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
