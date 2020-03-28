{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Control.Monad.Except as E
import           System.Environment (getArgs)
import           System.Exit (exitFailure, exitSuccess)
import           System.IO (getContents, hPrint, hPutStrLn, stderr, stdin)

import qualified Errors as Errs
import qualified Typechecker as TC
import           Javalette.Abs (Prog)
import           Javalette.ErrM (Err (Ok, Bad))
import           Javalette.Par (pProg, myLexer)

run :: String -> Either Errs.Error Prog
run code = do
  -- Lex
  let tokens = myLexer code

  -- Parse
  ast <- toEither $ pProg tokens

  -- Typecheck
  TC.runTypecheck $ TC.typecheck ast

  where
    toEither :: Err Prog -> Either Errs.Error Prog
    toEither = \case
      Ok ast     -> Right ast
      Bad errMsg -> Left $ Errs.Error errMsg

getInput :: IO String
getInput = do
  args <- getArgs
  case args of
    []     -> getContents
    [file] -> readFile file
    _      -> do
      hPutStrLn stderr "> Usage: jlc <Javalette source file>"
      exitFailure

main :: IO ()
main = do
  input <- getInput
  case run input of
    Right _ -> do
      hPutStrLn stderr "OK"
      exitSuccess

    Left err -> do
      hPutStrLn stderr "ERROR"
      hPrint stderr err
      exitFailure
