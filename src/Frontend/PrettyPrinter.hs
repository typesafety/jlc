{-# LANGUAGE LambdaCase #-}
 
{- | Module for prettyprinting Javalette ASTs, mainly for
debugging purposes.
-}

module Frontend.PrettyPrinter
       ( Pretty
       , prettyPrint
       ) where

import Data.Functor ((<&>))
import Data.List (intercalate)

import Javalette.Abs

import qualified Control.Monad.Reader as R


-- | Prettyprint something, using the given indentation level.
prettyPrint :: Pretty a => Int -> a -> String
prettyPrint n = flip runPrettyP n . pPrint

-- | PrettyP keeps the indentation size in its environment.
type PrettyP a = R.Reader Int a

runPrettyP :: PrettyP a -> Int -> a
runPrettyP = R.runReader

getIndent :: PrettyP String
getIndent = do
  indentSize <- R.ask
  pure $ replicate indentSize ' '

class Pretty a where
  pPrint :: a -> PrettyP String

instance Pretty Prog where
  pPrint (Program topDefs) = intercalate "\n" <$> mapM pPrint topDefs

instance Pretty TopDef where
  pPrint (FnDef typ ident args (Block stmts)) = do
    pTyp <- pPrint typ
    pId <- pPrint ident
    pArgs <- mapM pPrint args

    indent <- getIndent
    pStmts <- unlines . map (indent ++) . concatMap lines
      <$> mapM pPrint stmts

    pure $ mconcat
      [ pTyp, " ", pId, "(", intercalate ", " pArgs, ")\n"
      , "{\n"
      , pStmts
      , "}\n"
      ]

instance Pretty Blk where
  pPrint (Block stmts) = do
    let close = (++ ["}"])
    let open  = ("{" :)

    indent <- getIndent
    unlines . open . close . map (indent ++) . concatMap lines
      <$> mapM pPrint stmts

instance Pretty Arg where
  pPrint (Argument typ ident) =
    (((++) <$> pPrint typ <*> pure " ") <&> (++)) <*> pPrint ident

instance Pretty Ident where
  pPrint (Ident s) = pure s

instance Pretty Type where
  pPrint typ = case typ of
    Int      -> pure "int"
    Double   -> pure "double"
    Bool     -> pure "boolean"
    Void     -> pure "void"
    Arr t    -> pPrint t <&> (++ "[]")
    Str      -> pure "STRING"
    Fun t ts -> do
      pT <- pPrint t
      pTs <- intercalate "," <$> traverse pPrint ts
      pure $ "FUNCTION(" ++ pTs ++ ":" ++ pT

instance Pretty Var where
  pPrint (IdVar ident)          = pPrint ident
  pPrint (ArrVar ident arrIdxs) = do
    xs <- concat <$> traverse pPrint arrIdxs
    pPrint ident <&> (++ xs)

instance Pretty ArrIndex where
  pPrint (ArrIndex e) = ("[" ++) <$> pPrint e <&> (++ "]")

instance Pretty Item where
  pPrint (NoInit ident)    = pPrint ident
  pPrint (Init ident expr) =
    (pPrint ident <&> (++ " = ") <&> (++)) <*> pPrint expr

instance Pretty Stmt where
  pPrint = \case
    Empty -> pure ""

    BStmt blk -> pPrint blk

    Decl typ items -> do
      pTyp <- pPrint typ
      pItems <- intercalate ", " <$> mapM pPrint items
      pure $ mconcat [pTyp, " ", pItems, ";"]

    Ass ident expr ->
      (pPrint ident <&> (++ " = ") <&> (++)) <*> pPrint expr <&> (++ ";")

    Incr ident -> pPrint ident <&> (++ "++;")

    Decr ident -> pPrint ident <&> (++ "--;")

    Ret expr -> ("return " ++) <$> pPrint expr <&> (++ ";")

    VRet -> pure "return;"

    If expr stmt -> do
      pExpr <- pPrint expr
      pStmt <- pPrint stmt
      indent <- decideIndent stmt
      pure $ mconcat
        [ "if (", pExpr, ")\n"
        , indent, pStmt
        ]

    IfElse expr s1 s2 -> do
      pExpr <- pPrint expr
      pS1 <- pPrint s1
      pS2 <- pPrint s2
      indent1 <- decideIndent s1
      indent2 <- decideIndent s2
      pure $ mconcat
        [ "if (", pExpr, ")\n"
        , indent1, pS1, "\n"
        , "else\n"
        , indent2, pS2
        ]

    While expr stmt -> do
      pExpr <- pPrint expr
      pStmt <- pPrint stmt
      indent <- decideIndent stmt
      pure $ mconcat
        [ "while (", pExpr, ")\n"
        , indent, pStmt
        ]

    ForEach typ ident expr stmt -> do
      pTyp <- pPrint typ
      pIdent <- pPrint ident
      pExpr <- pPrint expr
      pStmt <- pPrint stmt
      indent <- decideIndent stmt
      pure $ mconcat
        [ "for (", pTyp, " ", pIdent, " : ", pExpr, ")\n"
        , indent, pStmt
        ]

    SExp expr -> pPrint expr <&> (++ ";")

    where
      -- Return the indentation if the given statement is not a block.
      decideIndent :: Stmt -> PrettyP String
      decideIndent = \case
        BStmt{} -> pure ""
        _       -> getIndent

instance Pretty Expr where
  pPrint = \case
    ENewArr typ expr -> do
      pTyp <- pPrint typ
      pExpr <- pPrint expr
      return $ mconcat ["new ", pTyp, "[", pExpr, "]"]

    ELength var -> pPrint var <&> (++ ".length")

    EVar var -> pPrint var

    ELitInt int       -> pure $ show int
    ELitDouble double -> pure $ show double

    ELitTrue  -> pure "true"
    ELitFalse -> pure "false"

    EApp ident exprs -> do
      pId    <- pPrint ident
      pExprs <- intercalate ", " <$> mapM pPrint exprs
      pure $ mconcat [pId, "(", pExprs, ")"]

    EString str -> pure $ show str

    Neg expr -> ("-" ++) <$> pPrint expr
    Not expr -> ("!" ++) <$> pPrint expr

    EMul e1 op e2 -> unwords <$> sequenceA [pPrint e1, pPrint op, pPrint e2]
    EAdd e1 op e2 -> unwords <$> sequenceA [pPrint e1, pPrint op, pPrint e2]
    ERel e1 op e2 -> unwords <$> sequenceA [pPrint e1, pPrint op, pPrint e2]

    EAnd e1 e2 -> unwords <$> sequenceA [pPrint e1, pure "&&", pPrint e2]
    EOr  e1 e2 -> unwords <$> sequenceA [pPrint e1, pure "||", pPrint e2]

    AnnExp expr _typ -> pPrint expr

instance Pretty MulOp where
  pPrint = pure . pretty
    where
      pretty :: MulOp -> String
      pretty = \case
        Times -> "*"
        Div   -> "/"
        Mod   -> "%"

instance Pretty AddOp where
  pPrint = pure . pretty
    where
      pretty :: AddOp -> String
      pretty = \case
        Plus  -> "+"
        Minus -> "-"

instance Pretty RelOp where
  pPrint = pure . pretty
    where
      pretty :: RelOp -> String
      pretty = \case
        LTH -> "<"
        LE  -> "<="
        GTH -> ">"
        GE  -> ">="
        EQU -> "=="
        NE  -> "!="

instance Pretty a => Pretty [a] where
  pPrint xs = do
    ys <- traverse pPrint xs
    pure . (++ "]") . ('[':) . intercalate "," $ ys
