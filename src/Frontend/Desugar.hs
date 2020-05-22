{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Module for running various desugaring conversions, keeping
the semantics intact but simplifying the syntax. This will be
useful in future compilation steps.

Desugaring should be run _after_ alpha-renaming. Some procedures
will assume that all variables are unique.

TODO: Desugaring into a new data type that explicitly does not include
      sugar-only expressions and statements allows for better use of
      the type system. The following phases will need to change their
      implmentations in that case.
-}

module Frontend.Desugar
       ( desugar
       ) where

import qualified Control.Monad.State.Strict as ST
import qualified Lens.Micro.Platform as L

import Javalette.Abs


newtype St = St
  { _stCounter :: Int  -- ^ Counter for new variables to keep the unique.
  }

$(L.makeLenses ''St)

type Desugar a = ST.State St a

runDesugar :: Desugar a -> a
runDesugar = flip ST.evalState initSt
  where
    initSt :: St
    initSt = St 0

desugar :: Prog -> Prog
desugar = runDesugar . desugar'
  where
    desugar' :: Prog -> Desugar Prog
    desugar' (Program topDefs) = Program <$> traverse dsgTopDef topDefs

dsgTopDef :: TopDef -> Desugar TopDef
dsgTopDef (FnDef typ ident args blk) = do
  desugaredBlk <- case typ of
    -- If the return type is void, we insert a return statement as
    -- the last statement. This makes code generation easier, and redundant
    -- return statements will be optimized away.
    Void -> dsgBlk blk >>= \ (Block ss) -> return $ Block (ss ++ [VRet])
    _    -> dsgBlk blk
  return $ FnDef typ ident args desugaredBlk

dsgBlk :: Blk -> Desugar Blk
dsgBlk (Block statements) = Block <$> dsgStmts statements
  where
    dsgStmts :: [Stmt] -> Desugar [Stmt]
    dsgStmts []       = pure []
    dsgStmts (s : ss) = dsgStmt s >>= \case
      Left stmts -> (stmts ++) <$> dsgStmts ss
      Right stmt -> (stmt :) <$> dsgStmts ss

-- | Desugar a statement. Because some desugaring procedures will require
-- additional statements, @dsgStmt@ can also return a list of statements.
dsgStmt :: Stmt -> Desugar (Either [Stmt] Stmt)
dsgStmt = \case
  BStmt blk -> Right . BStmt <$> dsgBlk blk

  -- Place all declarations on separate lines.
  Decl typ items -> Left . concat <$> mapM (dsgItem typ) items
    where
      -- Convert declarations into separate declarations and
      -- initializations when applicable.
      dsgItem :: Type -> Item -> Desugar [Stmt]
      dsgItem t i = case i of
        NoInit _        -> pure [Decl t [i]]
        Init ident expr -> do
          dExpr <- dsgExpr expr
          return
            [ Decl t [NoInit ident]
            , Ass (IdVar ident) dExpr
            ]

  Ass ident expr -> Right . Ass ident <$> dsgExpr expr

  -- Rewrite @x++; x--;@ to @x = x + 1; x = x - 1;@.
  Incr ident -> pure . Right $ Ass ident $ EAdd (EVar ident) Plus (ELitInt 1)
  Decr ident -> pure . Right $ Ass ident $ EAdd (EVar ident) Minus (ELitInt 1)

  Ret expr -> Right . Ret <$> dsgExpr expr

  If expr stmt -> Right
    <$> (If <$> dsgExpr expr <*> handleS stmt)

  IfElse expr s1 s2 -> Right
    <$> (IfElse <$> dsgExpr expr <*> handleS s1 <*> handleS s2)

  While expr stmt -> Right
    <$> (While <$> dsgExpr expr <*> handleS stmt)

  -- We can rewrite ForEach as a While-statement.
  ForEach typ ident expr stmt -> do
    counterId  <- nextIdent
    arrExprId  <- nextIdent

    let counterDecl = Decl Int [Init counterId (ELitInt 0)]
    let exprIdDecl  = Decl (Arr typ) [Init arrExprId expr]
    let condExpr    =
          ERel (EVar (IdVar counterId)) LTH (ELength (IdVar arrExprId))
    let incr        = Incr (IdVar counterId)
    let bindToId    =
          Decl typ
            [ Init
              ident
              (EVar (ArrVar arrExprId [ArrIndex (EVar (IdVar counterId))]))
            ]
    let new =
          [ counterDecl
          , exprIdDecl
          , While condExpr (BStmt (Block
              [ bindToId
              , stmt
              , incr
              ]
            ))
          ]
    Left . concatMap (either id (: [])) <$> mapM dsgStmt new

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

-- | Expression desugaring can be added here if applicable needed.
dsgExpr :: Expr -> Desugar Expr
dsgExpr = pure

--
-- * State-related helper functions
--

nextIdent :: Desugar Ident
nextIdent = do
  ident <- Ident . (identBase ++ ) . show <$> L.use stCounter
  L.modifying stCounter (+ 1)
  return ident

-- | Arbitrary prefix string that is not used elsewhere in the compiler.
identBase :: String
identBase = "i"
