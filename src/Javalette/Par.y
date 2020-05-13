-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Javalette.Par where
import Javalette.Abs
import Javalette.Lex
import Javalette.ErrM

}

%name pProg Prog
-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype {Token}
%token
  '!' { PT _ (TS _ 1) }
  '!=' { PT _ (TS _ 2) }
  '%' { PT _ (TS _ 3) }
  '&&' { PT _ (TS _ 4) }
  '(' { PT _ (TS _ 5) }
  ')' { PT _ (TS _ 6) }
  '*' { PT _ (TS _ 7) }
  '+' { PT _ (TS _ 8) }
  '++' { PT _ (TS _ 9) }
  ',' { PT _ (TS _ 10) }
  '-' { PT _ (TS _ 11) }
  '--' { PT _ (TS _ 12) }
  '.' { PT _ (TS _ 13) }
  '/' { PT _ (TS _ 14) }
  ':' { PT _ (TS _ 15) }
  '::' { PT _ (TS _ 16) }
  ';' { PT _ (TS _ 17) }
  '<' { PT _ (TS _ 18) }
  '<=' { PT _ (TS _ 19) }
  '=' { PT _ (TS _ 20) }
  '==' { PT _ (TS _ 21) }
  '>' { PT _ (TS _ 22) }
  '>=' { PT _ (TS _ 23) }
  'String' { PT _ (TS _ 24) }
  '[' { PT _ (TS _ 25) }
  '[]' { PT _ (TS _ 26) }
  ']' { PT _ (TS _ 27) }
  'boolean' { PT _ (TS _ 28) }
  'double' { PT _ (TS _ 29) }
  'else' { PT _ (TS _ 30) }
  'false' { PT _ (TS _ 31) }
  'for' { PT _ (TS _ 32) }
  'if' { PT _ (TS _ 33) }
  'int' { PT _ (TS _ 34) }
  'length' { PT _ (TS _ 35) }
  'new' { PT _ (TS _ 36) }
  'return' { PT _ (TS _ 37) }
  'true' { PT _ (TS _ 38) }
  'void' { PT _ (TS _ 39) }
  'while' { PT _ (TS _ 40) }
  '{' { PT _ (TS _ 41) }
  '||' { PT _ (TS _ 42) }
  '}' { PT _ (TS _ 43) }

L_ident  { PT _ (TV $$) }
L_integ  { PT _ (TI $$) }
L_doubl  { PT _ (TD $$) }
L_quoted { PT _ (TL $$) }


%%

Ident   :: { Ident }   : L_ident  { Ident $1 }
Integer :: { Integer } : L_integ  { (read ( $1)) :: Integer }
Double  :: { Double }  : L_doubl  { (read ( $1)) :: Double }
String  :: { String }  : L_quoted {  $1 }

Prog :: { Prog }
Prog : ListTopDef { Javalette.Abs.Program $1 }
TopDef :: { TopDef }
TopDef : Type Ident '(' ListArg ')' Blk { Javalette.Abs.FnDef $1 $2 $4 $6 }
ListTopDef :: { [TopDef] }
ListTopDef : TopDef { (:[]) $1 } | TopDef ListTopDef { (:) $1 $2 }
Arg :: { Arg }
Arg : Type Ident { Javalette.Abs.Argument $1 $2 }
ListArg :: { [Arg] }
ListArg : {- empty -} { [] }
        | Arg { (:[]) $1 }
        | Arg ',' ListArg { (:) $1 $3 }
Blk :: { Blk }
Blk : '{' ListStmt '}' { Javalette.Abs.Block (reverse $2) }
Var :: { Var }
Var : Ident { Javalette.Abs.IdVar $1 }
    | Ident ListArrIndex { Javalette.Abs.ArrVar $1 $2 }
ArrIndex :: { ArrIndex }
ArrIndex : '[' Expr ']' { Javalette.Abs.ArrIndex $2 }
ListArrIndex :: { [ArrIndex] }
ListArrIndex : ArrIndex { (:[]) $1 }
             | ArrIndex ListArrIndex { (:) $1 $2 }
Stmt :: { Stmt }
Stmt : ';' { Javalette.Abs.Empty }
     | Blk { Javalette.Abs.BStmt $1 }
     | Type ListItem ';' { Javalette.Abs.Decl $1 $2 }
     | Var '=' Expr ';' { Javalette.Abs.Ass $1 $3 }
     | Var '++' ';' { Javalette.Abs.Incr $1 }
     | Var '--' ';' { Javalette.Abs.Decr $1 }
     | 'return' Expr ';' { Javalette.Abs.Ret $2 }
     | 'return' ';' { Javalette.Abs.VRet }
     | 'if' '(' Expr ')' Stmt { Javalette.Abs.If $3 $5 }
     | 'if' '(' Expr ')' Stmt 'else' Stmt { Javalette.Abs.IfElse $3 $5 $7 }
     | 'while' '(' Expr ')' Stmt { Javalette.Abs.While $3 $5 }
     | 'for' '(' Type Ident ':' Expr ')' Stmt { Javalette.Abs.ForEach $3 $4 $6 $8 }
     | Expr ';' { Javalette.Abs.SExp $1 }
ListStmt :: { [Stmt] }
ListStmt : {- empty -} { [] } | ListStmt Stmt { flip (:) $1 $2 }
Item :: { Item }
Item : Ident { Javalette.Abs.NoInit $1 }
     | Ident '=' Expr { Javalette.Abs.Init $1 $3 }
ListItem :: { [Item] }
ListItem : Item { (:[]) $1 } | Item ',' ListItem { (:) $1 $3 }
Type1 :: { Type }
Type1 : 'int' { Javalette.Abs.Int }
      | 'double' { Javalette.Abs.Double }
      | 'boolean' { Javalette.Abs.Bool }
      | 'void' { Javalette.Abs.Void }
      | '(' Type ')' { $2 }
Type :: { Type }
Type : Type '[]' { Javalette.Abs.Arr $1 } | Type1 { $1 }
ListType :: { [Type] }
ListType : {- empty -} { [] }
         | Type { (:[]) $1 }
         | Type ',' ListType { (:) $1 $3 }
Expr7 :: { Expr }
Expr7 : Var '.' 'length' { Javalette.Abs.ELength $1 }
      | Var { Javalette.Abs.EVar $1 }
      | Integer { Javalette.Abs.ELitInt $1 }
      | Double { Javalette.Abs.ELitDouble $1 }
      | 'true' { Javalette.Abs.ELitTrue }
      | 'false' { Javalette.Abs.ELitFalse }
      | Ident '(' ListExpr ')' { Javalette.Abs.EApp $1 $3 }
      | String { Javalette.Abs.EString $1 }
      | '(' Expr ')' { $2 }
Expr6 :: { Expr }
Expr6 : 'new' Type '[' Expr7 ']' { Javalette.Abs.ENewArr $2 $4 }
      | '-' Expr7 { Javalette.Abs.Neg $2 }
      | '!' Expr7 { Javalette.Abs.Not $2 }
      | Expr7 { $1 }
Expr5 :: { Expr }
Expr5 : Expr5 MulOp Expr6 { Javalette.Abs.EMul $1 $2 $3 }
      | Expr6 { $1 }
Expr4 :: { Expr }
Expr4 : Expr4 AddOp Expr5 { Javalette.Abs.EAdd $1 $2 $3 }
      | Expr5 { $1 }
Expr3 :: { Expr }
Expr3 : Expr3 RelOp Expr4 { Javalette.Abs.ERel $1 $2 $3 }
      | Expr4 { $1 }
Expr2 :: { Expr }
Expr2 : Expr3 '&&' Expr2 { Javalette.Abs.EAnd $1 $3 }
      | Expr3 { $1 }
Expr1 :: { Expr }
Expr1 : Expr2 '||' Expr1 { Javalette.Abs.EOr $1 $3 } | Expr2 { $1 }
Expr :: { Expr }
Expr : Expr1 { $1 }
ListExpr :: { [Expr] }
ListExpr : {- empty -} { [] }
         | Expr { (:[]) $1 }
         | Expr ',' ListExpr { (:) $1 $3 }
AddOp :: { AddOp }
AddOp : '+' { Javalette.Abs.Plus } | '-' { Javalette.Abs.Minus }
MulOp :: { MulOp }
MulOp : '*' { Javalette.Abs.Times }
      | '/' { Javalette.Abs.Div }
      | '%' { Javalette.Abs.Mod }
RelOp :: { RelOp }
RelOp : '<' { Javalette.Abs.LTH }
      | '<=' { Javalette.Abs.LE }
      | '>' { Javalette.Abs.GTH }
      | '>=' { Javalette.Abs.GE }
      | '==' { Javalette.Abs.EQU }
      | '!=' { Javalette.Abs.NE }
{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
}

