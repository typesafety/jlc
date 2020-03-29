{-# LANGUAGE LambdaCase #-}

module Errors where

import Data.Char (toLower)

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

  -- When a statement is a single expression but not of type void.
  -- (Expression) (Inferred type of expression)
  | NonVoidSExpError Expr Type

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

    NonVoidSExpError exp inferredType -> mconcat
      [ "Statements consisting of single expressions must be of type void,"
      , " but the following expression:\n"
      , "    ", show exp, "\n"
      , "had type:\n"
      , "    ", showType inferredType
      ]

    Error str -> str

    where
      showId :: Ident -> String
      showId (Ident str) = str

      showType :: Type -> String
      showType = \case
        Fun retType argTypes -> "function of type " ++ show retType
        t -> show t
