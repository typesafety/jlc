{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)

import Javalette.Abs (Prog)
import Javalette.ErrM (Err (Ok, Bad))
import Javalette.Par (pProg, myLexer)

import qualified Typechecker as TC

parse :: String -> Err Prog
parse s = pProg $ myLexer s

typecheck :: Prog -> Err Prog
typecheck = TC.typecheck

run :: String -> Err Prog
run code = do
  prog <- parse code
  typecheck prog

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- Only supports a single file currently
    [file] -> run <$> readFile file >>= \case
      Ok a    -> putStrLn "OK."    >> print a
      Bad err -> putStrLn "ERROR." >> error err

    _      -> putStrLn "> Run on a Javalette source file"
