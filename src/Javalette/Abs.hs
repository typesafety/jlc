

module Javalette.Abs where

-- Haskell module generated by the BNF converter




newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
data Prog = Program [TopDef]
  deriving (Eq, Ord, Show, Read)

data TopDef = FnDef Type Ident [Arg] Blk
  deriving (Eq, Ord, Show, Read)

data Arg = Argument Type Ident
  deriving (Eq, Ord, Show, Read)

data Blk = Block [Stmt]
  deriving (Eq, Ord, Show, Read)

data Var = IdVar Ident | ArrVar Ident [ArrIndex]
  deriving (Eq, Ord, Show, Read)

data ArrIndex = ArrIndex Expr
  deriving (Eq, Ord, Show, Read)

data Stmt
    = Empty
    | BStmt Blk
    | Decl Type [Item]
    | Ass Var Expr
    | Incr Var
    | Decr Var
    | Ret Expr
    | VRet
    | If Expr Stmt
    | IfElse Expr Stmt Stmt
    | While Expr Stmt
    | ForEach Type Ident Expr Stmt
    | SExp Expr
  deriving (Eq, Ord, Show, Read)

data Item = NoInit Ident | Init Ident Expr
  deriving (Eq, Ord, Show, Read)

data Type
    = Int | Double | Bool | Void | Arr Type | Fun Type [Type] | Str
  deriving (Eq, Ord, Show, Read)

data Expr
    = ELength Var
    | EVar Var
    | ELitInt Integer
    | ELitDouble Double
    | ELitTrue
    | ELitFalse
    | EApp Ident [Expr]
    | EString String
    | ENewArr Type Expr
    | Neg Expr
    | Not Expr
    | EMul Expr MulOp Expr
    | EAdd Expr AddOp Expr
    | ERel Expr RelOp Expr
    | EAnd Expr Expr
    | EOr Expr Expr
    | AnnExp Expr Type
  deriving (Eq, Ord, Show, Read)

data AddOp = Plus | Minus
  deriving (Eq, Ord, Show, Read)

data MulOp = Times | Div | Mod
  deriving (Eq, Ord, Show, Read)

data RelOp = LTH | LE | GTH | GE | EQU | NE
  deriving (Eq, Ord, Show, Read)

