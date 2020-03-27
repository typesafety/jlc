{-# LANGUAGE LambdaCase #-}

module Errors where

import Data.Char (toLower)

import Javalette.Abs

data Error
  -- When the type of the return value of a function does not match
  -- the type of the actual return value.
  -- (Name of function) (Expected return type) (Actual return type)
  = ReturnError Ident Type Type

  -- When some variable is not found in the current context.
  -- (Name of variable)
  | SymbolError Ident

  -- When some expression has the incorrect inferred type.
  -- (Expression) (Allowed types) (Actual inferred type) 
  | ExpError Expr [Type] Type 

  -- Generic error message.
  -- (Error message)
  | Error String

instance Show Error where
  -- TODO
  show = \case
    ReturnError id expectedType actualType -> mconcat
      [ "Function `", showId id, "` has return type:\n"
      , "    ", showType expectedType, "\n"
      , "but incorrectly returns:\n"
      , "    ", showType actualType
      ]

    ExpError exp expectedTypes inferredType -> mconcat
      [ "Expected expression\n"
      , "    ", show exp, "\n"
      , "to have one of the following types:\n"
      , "    ", show $ map showType expectedTypes, "\n"
      , "but instead the inferred type was:\n"
      , "    ", showType inferredType
      ]

    SymbolError id -> mconcat
      [ "Could not resolve symbol: `", showId id, "`"
      ]

    Error str -> str

    where
      showId :: Ident -> String
      showId (Ident str) = str

      showType :: Type -> String
      showType = \case
        Fun retType argTypes -> "function of type " ++ show retType
        t -> map toLower . show $ t
