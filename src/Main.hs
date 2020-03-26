{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Control.Monad.Except as E
import           System.Environment (getArgs)
import           System.Exit (exitFailure)

import Javalette.Abs (Prog)
import Javalette.Par (pProg, myLexer)

import qualified Errors as Errs
import qualified Typechecker as TC

run :: String -> Either Errs.Error Prog
run code = do
  -- Lex
  let tokens = myLexer code

  undefined
  -- -- Parse
  --  pProg tokens


  -- -- Typecheck
  -- TC.typecheck ast

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- Only supports a single file currently
    [file] -> run <$> readFile file >>= \case
      Right a  -> putStrLn "OK."    >> putStrLn "hasdf" -- print a
      Left err -> putStrLn "ERROR." >> error (show err)

    _      -> do
      putStrLn "> Run on a Javalette source file"
      exitFailure
