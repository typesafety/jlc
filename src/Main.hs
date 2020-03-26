{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)

import Javalette.Abs (Prog)
import Javalette.ErrM (Err (Ok, Bad))
import Javalette.Par (pProg, myLexer)

import qualified Typechecker as TC

run :: String -> Err Prog
run code = do
  -- Lex
  let tokens = myLexer code

  -- Parse
  ast <- pProg tokens

  -- Typecheck
  TC.typecheck ast

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- Only supports a single file currently
    [file] -> run <$> readFile file >>= \case
      Ok a    -> putStrLn "OK."    >> print a
      Bad err -> putStrLn "ERROR." >> error err

    _      -> putStrLn "> Run on a Javalette source file"
