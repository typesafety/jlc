{-# LANGUAGE LambdaCase #-}
 
-- | Module for prettyprinting Javalette ASTs, mainly for
-- debugging purposes.

module PrettyPrinter
  ( Pretty
  , prettyPrint
  ) where

import           Data.Bifunctor (first, second)
import           Data.Functor ((<&>))
import           Data.List (intercalate, intersperse)
import qualified Control.Monad.Reader as R

import           Javalette.Abs

-- | Prettyprint something, using the given indentation.
prettyPrint :: Pretty a => Int -> a -> String
prettyPrint n = flip runPrettyP n . pPrint

type PrettyP a = R.Reader (Int, Int) a

runPrettyP :: PrettyP a -> Int -> a
runPrettyP p indentSize = R.runReader p (indentSize, 0 :: Int)

indentSize :: PrettyP Int
indentSize = R.asks fst

indent :: PrettyP String
indent = flip replicate ' ' <$> R.asks snd

increaseIndent :: (Int, Int) -> (Int, Int)
increaseIndent (size, curr) = (size, curr + size)

class Pretty a where
  pPrint :: a -> PrettyP String

instance Pretty Prog where
  pPrint (Program topDefs) = intercalate "\n" <$> mapM pPrint topDefs

instance Pretty TopDef where
  pPrint (FnDef typ id args blk) = do
    indent <- R.ask
    pTyp <- pPrint typ
    pId <- pPrint id
    pArgs <- mapM pPrint args
    pBlk <- R.local increaseIndent $ pPrint blk
    pure $ mconcat
      [ pTyp, " ", pId, "(", intercalate ", " pArgs, ") {\n"
      , pBlk, "\n"
      , "}"
      ]

instance Pretty Blk where
  pPrint (Block stmts) = intercalate "\n" <$> mapM pPrint stmts

instance Pretty Arg where
  pPrint (Argument typ id) =
    (((++) <$> pPrint typ <*> pure " ") <&> (++)) <*> pPrint id

instance Pretty Ident where
  pPrint (Ident s) = pure s

instance Pretty Type where
  pPrint typ = pure $ case typ of
    Int -> "int"
    Double -> "double"
    Bool -> "boolean"
    Void -> "void"
    Fun t ts -> "FUNTYPE"
    Str -> "STRINGTYPE"

instance Pretty Item where
  pPrint (NoInit id)    = pPrint id
  pPrint (Init id expr) = (pPrint id <&> (++ " = ") <&> (++)) <*> pPrint expr

instance Pretty Stmt where
  pPrint = \case
    Empty -> pure ""

    BStmt blk ->
      ("{\n" ++) <$> R.local increaseIndent (pPrint blk) <&> (++ "\n}\n")

    Decl typ items -> do
      pTyp <- pPrint typ
      pItems <- intercalate ", " <$> mapM pPrint items
      pure $ mconcat [pTyp, " ", pItems, ";"]

    Ass id expr -> (pPrint id <&> (++ " = ") <&> (++)) <*> pPrint expr

    Incr id -> pPrint id <&> (++ "++;")

    Decr id -> pPrint id <&> (++ "--;")

    Ret expr -> ("return " ++) <$> pPrint expr <&> (++ ";")

    VRet -> pure "return;"

    If expr stmt -> do
      pExpr <- pPrint expr
      pStmt <- R.local increaseIndent $ pPrint stmt
      pure $ mconcat
        [ "if (", pExpr, ")\n"
        , pStmt, "\n"
        ]

    IfElse expr s1 s2 -> do
      pExpr <- pPrint expr
      pS1 <- R.local increaseIndent $ pPrint s1
      pS2 <- R.local increaseIndent $ pPrint s2
      pure $ mconcat
        [ "if (", pExpr, ")\n"
        , pS1, "\n"
        , "else\n"
        , pS2, "\n"
        ]

    While expr stmt -> do
      pExpr <- pPrint expr
      pStmt <- pPrint stmt
      pure $ mconcat
        [ "while (", pExpr, ")\n"
        , pStmt, "\n"
        ]

    SExp expr -> pPrint expr <&> (++ ";")

instance Pretty Expr where
  pPrint = \case
    EVar id -> pPrint id

    ELitInt int       -> pure $ show int
    ELitDouble double -> pure $ show double

    ELitTrue  -> pure "true"
    ELitFalse -> pure "false"

    EApp id exprs -> do
      pId    <- pPrint id
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
