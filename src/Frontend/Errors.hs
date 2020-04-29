{-# LANGUAGE LambdaCase #-}

module Frontend.Errors
       ( Error (..)
       , compilerErrMsg
       ) where

import Javalette.Abs
import Frontend.PrettyPrinter (Pretty, prettyPrint)

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
  | IncrTypeError Ident Type

  -- When attempting to decrement a non-integer variable.
  -- (Name of variable) (Actual type)
  | DecrTypeError Ident Type

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

  -- Generic, but main()-related error message.
  -- (Error message)
  | MainError String

  -- Generic error message.
  -- (Error message)
  | Error String

instance Show Error where
  show = \case
    ReturnError id expectedType actualType -> mconcat
      [ "Function `", pp id, "` has return type:\n"
      , "    ", pp expectedType, "\n"
      , "but incorrectly returns:\n"
      , "    ", pp actualType
      ]

    MissingReturnError id -> mconcat
      [ "Function `", pp id, "`: missing reachable return statement"
      ]

    IncrTypeError id typ -> mconcat
      [ "Incrementing (++) ", pp id, " requires type"
      , "int, but instead got type: ", pp typ
      ]

    DecrTypeError id typ -> mconcat
      [ "Decrementing (--) ", pp id, " requires type"
      , "int, but instead got type: ", pp typ
      ]

    SymbolError id -> mconcat
      [ "Could not resolve symbol: `", pp id, "`"
      ]

    NumArgsError id expectedNum actualNum -> mconcat
      [ "Function `", pp id, "` expected "
      , show expectedNum, " arguments, but got ", show actualNum
      ]

    ExpError exp expectedTypes inferredType -> mconcat
      [ "Expected expression\n"
      , "    ", pp exp, "\n"
      , "to have one of the following types:\n"
      , "    ", init . tail . pp $ expectedTypes, "\n"
      , "but instead the inferred type was:\n"
      , "    ", pp inferredType
      ]

    DuplicateDeclError id -> mconcat
      [ "Variable `", pp id, "` is already declared in"
      , " the current context"
      ]

    DuplicateFunError id -> mconcat
      [ "Duplicate top-level function identifier: `", pp id, "`"
      ]

    DuplicateParamError id -> mconcat
      [ "Duplicate argument identifiers in function: `", pp id, "`"
      ]

    VoidParamError id -> mconcat
      [ "Function: `", pp id, "` has parameter(s) of type void"
      ]

    NonVoidSExpError exp inferredType -> mconcat
      [ "Statements consisting of single expressions must be of type void,"
      , " but the following expression:\n"
      , "    ", pp exp, "\n"
      , "had type:\n"
      , "    ", pp inferredType
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