{-# LANGUAGE LambdaCase #-}

module Frontend.Errors
       ( Error (..)
       , compilerErrMsg
       ) where

import Frontend.PrettyPrinter (Pretty, prettyPrint)
import Javalette.Abs

import qualified GHC.Stack as Stack


data Error
  -- When the type of the return value of a function does not match
  -- the type of the actual return value.
  -- (Name of function) (Expected return type) (Actual return type)
  = ReturnError Ident Type Type

  -- When a return statement could not be found or reached in
  -- a function.
  -- (Name of function)
  | MissingReturnError Ident

  -- When attempting to increment a non-integer variable.
  -- (Name of variable) (Actual type)
  | IncrTypeError Var Type

  -- When attempting to decrement a non-integer variable.
  -- (Name of variable) (Actual type)
  | DecrTypeError Var Type

  -- When a variable or function is not found in the current context.
  -- (Name of variable/function)
  | SymbolError Ident

  -- When a function call is given the incorrect number of arguments.
  -- (Name of function) (Expected number of args) (Actual number of args)
  | NumArgsError Ident Int Int

  -- When a expression has the incorrect inferred type.
  -- (Expression) (Allowed types) (Actual inferred type) 
  | ExpError Expr [Type] Type

  -- When attempting to declare a variable that already exists in
  -- the current context.
  -- (Name of variable)
  | DuplicateDeclError Ident

  -- When there exist multiple top-level function definitions with
  -- identical names.
  -- (Name of function)
  | DuplicateFunError Ident

  -- When a function has multiple parameters with identical names.
  -- (Name of function)
  | DuplicateParamError Ident

  -- When a function has one or more parameters of type void.
  -- (Name of function)
  | VoidParamError Ident

  -- When a statement is a single expression but not of type void.
  -- (Expression) (Inferred type of expression)
  | NonVoidSExpError Expr Type

  -- When an array-only operation (loop, length) is called on a variable
  -- that is not of type array.
  -- (Expression, expected array type) (Actual inferred type)
  | NonArrayError Expr Type

  -- Generic error for badly formed declarations.
  | DeclFormError Item

  -- Generic, but main()-related error message.
  -- (Error message)
  | MainError String

  -- Generic error message.
  -- (Error message)
  | Error String

instance Show Error where
  show = \case
    ReturnError ident expectedType actualType -> mconcat
      [ "Function `", pp ident, "` has return type:\n"
      , "    ", pp expectedType, "\n"
      , "but incorrectly returns:\n"
      , "    ", pp actualType
      ]

    MissingReturnError ident -> mconcat
      [ "Function `", pp ident, "`: missing reachable return statement"
      ]

    IncrTypeError var typ -> mconcat
      [ "Incrementing (++) ", pp var, " requires type"
      , "int, but instead got type: ", pp typ
      ]

    DecrTypeError var typ -> mconcat
      [ "Decrementing (--) ", pp var, " requires type"
      , "int, but instead got type: ", pp typ
      ]

    SymbolError ident -> mconcat
      [ "Could not resolve symbol: `", pp ident, "`"
      ]

    NumArgsError ident expectedNum actualNum -> mconcat
      [ "Function `", pp ident, "` expected "
      , show expectedNum, " arguments, but got ", show actualNum
      ]

    ExpError expr expectedTypes inferredType -> mconcat
      [ "Expected expression\n"
      , "    ", pp expr, "\n"
      , "to have one of the following types:\n"
      , "    ", init . tail . pp $ expectedTypes, "\n"
      , "but instead the inferred type was:\n"
      , "    ", pp inferredType
      ]

    DuplicateDeclError ident -> mconcat
      [ "Variable `", pp ident, "` is already declared in"
      , " the current context"
      ]

    DuplicateFunError ident -> mconcat
      [ "Duplicate top-level function identifier: `", pp ident, "`"
      ]

    DuplicateParamError ident -> mconcat
      [ "Duplicate argument identifiers in function: `", pp ident, "`"
      ]

    VoidParamError ident -> mconcat
      [ "Function: `", pp ident, "` has parameter(s) of type void"
      ]

    NonVoidSExpError expr inferredType -> mconcat
      [ "Statements consisting of single expressions must be of type void,"
      , " but the following expression:\n"
      , "    ", pp expr, "\n"
      , "had type:\n"
      , "    ", pp inferredType
      ]

    NonArrayError expr inferredType -> mconcat
      [ "Expected the following expression have an array type:\n"
      , "    ", pp expr
      , "but the actual type was:\n"
      , "    ", pp inferredType
      ]

    DeclFormError item -> mconcat
      [ "Badly formed declaration of type:\n"
      , "    ", pp item
      ]

    MainError str -> str

    Error str -> str

    where
      pp :: Pretty a => a -> String
      pp = prettyPrint 4

compilerErrMsg :: Stack.HasCallStack => String
compilerErrMsg = mconcat
  [ ">>\n"
  , ">> An error has occurred in the compiler;\n"
  , ">> this is a compiler bug.\n"
  , ">>\n"
  , "Call stack:\n"
  , Stack.prettyCallStack Stack.callStack
  ]
