{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE DeriveFunctor #-}

{- | Module for running various desugaring conversions, keeping
the semantics intact but simplifying the syntax. This will be
useful in future compilation steps.

Desugaring should be run _after_ alpha-renaming. Some procedures
will assume that all variables are unique.

TODO: Should this desugaring module actually perform optimizations like
if (true)
  stmt;
->
stmt;

Or
!true -> false

or is that optimization that is better left to the LLVM optimizer?
-}

module Desugar
  ( desugar
  ) where


import           Javalette.Abs


newtype Desugar a = Desugar a
  deriving (Functor)

instance Applicative Desugar where
  pure = Desugar
  Desugar f <*> aa = fmap f aa

instance Monad Desugar where
  return  = pure
  m >>= f = f . runDesugar $ m

runDesugar :: Desugar a -> a
runDesugar (Desugar a) = a

desugar :: Prog -> Prog
desugar = runDesugar . desugar'
  where
    desugar' :: Prog -> Desugar Prog
    desugar' (Program topDefs) = Program <$> traverse dsgTopDef topDefs

dsgTopDef :: TopDef -> Desugar TopDef
dsgTopDef (FnDef typ id args blk) = FnDef typ id args <$> dsgBlk blk

dsgBlk :: Blk -> Desugar Blk
dsgBlk (Block stmts) = Block <$> dsgStmts stmts
  where
    dsgStmts :: [Stmt] -> Desugar [Stmt]
    dsgStmts []       = pure []
    dsgStmts (s : ss) = dsgStmt s >>= \case
      Left stmts -> (stmts ++) <$> dsgStmts ss
      Right stmt -> (stmt :) <$> dsgStmts ss

dsgItem :: Item -> Desugar Item
dsgItem (Init id expr) = Init id <$> dsgExpr expr
dsgItem item           = pure item

-- | Desugar a statement. Because some desugaring procedures will require
-- additional statements, @dsgStmt@ can also return a list of statements.
dsgStmt :: Stmt -> Desugar (Either [Stmt] Stmt)
dsgStmt = \case
  BStmt blk -> Right . BStmt <$> dsgBlk blk

  -- Place all declarations on separate lines. More verbose but simpler.
  -- (But now we get singleton item lists, which looks a bit ugly,
  -- maybe that can be addressed somehow?)
  Decl typ items -> do
    let d_Items = traverse dsgItem items
    if length items == 1
      then Right . Decl typ <$> d_Items
      else Left . map (Decl typ . (: [])) <$> d_Items

  Ass id expr -> Right . Ass id <$> dsgExpr expr

  -- Rewrite @x++; x--;@ to @x = x + 1; x = x - 1;@.
  Incr id -> pure . Right $ Ass id $ EAdd (EVar id) Plus (ELitInt 1)
  Decr id -> pure . Right $ Ass id $ EAdd (EVar id) Minus (ELitInt 1)

  Ret expr -> Right . Ret <$> dsgExpr expr

  -- For If/Else where the conditional is literal True or False,
  -- we can simplify greatly.
  If ELitTrue stmt -> Right <$> handleS stmt
  If ELitFalse _   -> Left  <$> pure []
  If expr stmt     -> Right <$> (If <$> dsgExpr expr <*> handleS stmt)

  IfElse ELitTrue s1 _  -> Right <$> handleS s1
  IfElse ELitFalse _ s2 -> Right <$> handleS s2
  IfElse expr s1 s2     -> Right
    <$> (IfElse <$> dsgExpr expr <*> handleS s1 <*> handleS s2)

  While expr stmt -> Right <$> (While <$> dsgExpr expr <*> handleS stmt)

  SExp expr -> Right . SExp <$> dsgExpr expr

  -- Catch-all
  stmt -> pure . Right $ stmt

  where
    -- Handle desugaring of the statements in If/Else/While-clauses:
    -- If the statement is a BStmt, we desugar it as expected,
    -- but if it is some other Stmt we need to handle the case where
    -- the desugaring creates a list of statements, and if so, wrap
    -- them in a Block.
    handleS :: Stmt -> Desugar Stmt
    handleS (BStmt blk) = BStmt <$> dsgBlk blk
    handleS stmt        = either (BStmt . Block) id <$> dsgStmt stmt

-- TODO: Determine if expressions like !true should be simplified
-- to false, or if this is inappropriate for desugaring.
-- Also, determine if -n should be rewritten to n * (-1); is it
-- an improvement?
dsgExpr :: Expr -> Desugar Expr
dsgExpr = pure
