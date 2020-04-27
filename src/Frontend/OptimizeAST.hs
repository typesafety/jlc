{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE DeriveFunctor #-}

{- | Module for running various optimizations before code generation,
through static analysis of the desugared AST.

Note that this should be run _after_ desugaring; there is an assumption
that certain constructs have been desugared away (x++ and x-- for example).
In the future, it might be better to use a different datatype for the
desugared and optimized program so that the types and their constructors
tell which constructs still exist after preprocessing.
-}

module Frontend.OptimizeAST
  ( optimizeAst
  ) where


import qualified GHC.Stack as Stack

import           Frontend.Errors (compilerErrMsg)
import           Javalette.Abs


newtype Optimize a = Optimize a
  deriving (Functor)

instance Applicative Optimize where
  pure = Optimize
  Optimize f <*> aa = fmap f aa

instance Monad Optimize where
  return  = pure
  m >>= f = f . runOptimize $ m

runOptimize :: Optimize a -> a
runOptimize (Optimize a) = a

-- We run optimization once, then once more on the optimized AST. This
-- is because it is the easiest way to potentially catch cases like
-- @ if (!(!true)) { stmt } @
-- without having a complex optStmt function, even if it is a bit
-- inefficient.
optimizeAst :: Prog -> Prog
optimizeAst p = runOptimize $ optProg p >>= optProg
  where
    optProg :: Prog -> Optimize Prog
    optProg (Program topDefs) = Program <$> traverse optTopDef topDefs

optTopDef :: TopDef -> Optimize TopDef
optTopDef (FnDef typ id args blk) = FnDef typ id args <$> optBlk blk

-- TODO: We might be able to perform some optimization here;
-- if we find a Ret or VRet in a block statement, we should be able
-- to remove all statements following it. Should work since we don't
-- have GOTOs in Javalette.
optBlk :: Blk -> Optimize Blk
optBlk (Block stmts) = Block <$> traverse optStmt stmts

optItem :: Item -> Optimize Item
optItem (Init id expr) = Init id <$> optExpr expr
optItem item           = pure item

optStmt :: Stack.HasCallStack => Stmt -> Optimize Stmt
optStmt = \case
  BStmt blk -> BStmt <$> optBlk blk

  Decl typ items -> Decl typ <$> traverse optItem items

  Ass id expr -> Ass id <$> optExpr expr

  Ret expr -> Ret <$> optExpr expr

  -- For If/Else where the conditional is literal True or False, we
  -- can simplify by removing the conditional part and possibly the block.
  If ELitTrue stmt -> optStmt stmt
  If ELitFalse _   -> pure Empty
  If expr stmt     -> If <$> optExpr expr <*> optStmt stmt

  IfElse ELitTrue s1 _  -> optStmt s1
  IfElse ELitFalse _ s2 -> optStmt s2
  IfElse expr s1 s2     ->
    IfElse <$> optExpr expr <*> optStmt s1 <*> optStmt s2

  While expr stmt -> While <$> optExpr expr <*> optStmt stmt

  SExp expr -> SExp <$> optExpr expr

  -- Other cases and unexpected cases.
  VRet   -> pure VRet
  Empty  -> pure Empty
  Incr{} -> error $ compilerErrMsg ++ "\n Encountered Incr in optStmt"
  Decr{} -> error $ compilerErrMsg ++ "\n Encountered Decr in optStmt"

optExpr :: Expr -> Optimize Expr
optExpr = \case
  EApp id exprs -> EApp id <$> traverse optExpr exprs

  Neg expr -> do
    oExpr <- optExpr expr
    optExpr $ EMul oExpr Times (ELitInt (-1))

  Not ELitTrue   -> pure ELitFalse
  Not ELitFalse  -> pure ELitTrue
  Not (Not expr) -> optExpr expr

  EMul e1 op e2 -> flip EMul op <$> optExpr e1 <*> optExpr e2
  EAdd e1 op e2 -> flip EAdd op <$> optExpr e1 <*> optExpr e2
  ERel e1 op e2 -> flip ERel op <$> optExpr e1 <*> optExpr e2

  EAnd e1 e2 -> EAnd <$> optExpr e1 <*> optExpr e2
  EOr  e1 e2 -> EOr  <$> optExpr e1 <*> optExpr e2

  -- Catch-all
  expr -> pure expr
