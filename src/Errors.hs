{-# LANGUAGE LambdaCase #-}

module Errors where

import Data.Char (toLower)

import Javalette.Abs

data Error
  -- When the type of the return value of a function does not match
  -- the type of the actual return value.
  -- (Name of function) (Expected return type) (Actual return type)
  = ReturnError Ident Type Type

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

    Error str -> str

    where
      showId :: Ident -> String
      showId (Ident str) = str

      showType :: Type -> String
      showType = \case
        Fun retType argTypes -> "function of type " ++ show retType
        t -> show t
