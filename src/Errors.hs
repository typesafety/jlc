{-# LANGUAGE LambdaCase #-}

module Errors where

import Javalette.Abs

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
      [ "Function `", showId id, "` has return type:\n"
      , "    ", showType expectedType, "\n"
      , "but incorrectly returns:\n"
      , "    ", showType actualType
      ]

    MissingReturnError id -> mconcat
      [ "Function `", showId id, "`: missing reachable return statement"
      ]

    IncrTypeError id typ -> mconcat
      [ "Incrementing (++) ", showId id, " requires type"
      , "int, but instead got type: ", showType typ
      ]

    DecrTypeError id typ -> mconcat
      [ "Decrementing (--) ", showId id, " requires type"
      , "int, but instead got type: ", showType typ
      ]

    SymbolError id -> mconcat
      [ "Could not resolve symbol: `", showId id, "`"
      ]

    NumArgsError id expectedNum actualNum -> mconcat
      [ "Function `", showId id, "` expected "
      , show expectedNum, " arguments, but got ", show actualNum
      ]

    ExpError exp expectedTypes inferredType -> mconcat
      [ "Expected expression\n"
      , "    ", show exp, "\n"
      , "to have one of the following types:\n"
      , "    ", show $ map showType expectedTypes, "\n"
      , "but instead the inferred type was:\n"
      , "    ", showType inferredType
      ]

    DuplicateDeclError id -> mconcat
      [ "Variable `", showId id, "` is already declared in"
      , " the current context"
      ]

    DuplicateFunError id -> mconcat
      [ "Duplicate top-level function identifier: `", showId id, "`"
      ]

    DuplicateParamError id -> mconcat
      [ "Duplicate argument identifiers in function: `", showId id, "`"
      ]

    VoidParamError id -> mconcat
      [ "Function: `", showId id, "` has parameter(s) of type void"
      ]

    NonVoidSExpError exp inferredType -> mconcat
      [ "Statements consisting of single expressions must be of type void,"
      , " but the following expression:\n"
      , "    ", show exp, "\n"
      , "had type:\n"
      , "    ", showType inferredType
      ]

    MainError str -> str

    Error str -> str

    where
      showId :: Ident -> String
      showId (Ident str) = str

      showType :: Type -> String
      showType = \case
        Fun retType _argTypes -> "function of type " ++ show retType
        t -> show t
