{-# LANGUAGE LambdaCase #-}

module Main where

import           System.Environment (getArgs)
import           System.Exit (exitFailure, exitSuccess)
import           System.IO (getContents, hPrint, hPutStrLn, stderr, stdin)

import qualified AlphaRename
import qualified CodeGenerator
import qualified Errors
import qualified PrettyPrinter
import qualified Typechecker

import           Javalette.Abs (Prog)
import           Javalette.ErrM (Err (Ok, Bad))
import           Javalette.Par (pProg, myLexer)

run :: String -> Either Errors.Error Prog
run code = do
  -- Lex
  let tokens = myLexer code

  -- Parse
  ast <- toEither $ pProg tokens

  -- Typecheck
  annotated <- Typechecker.typecheck ast

  return $ AlphaRename.alphaRename annotated

  where
    toEither :: Err Prog -> Either Errors.Error Prog
    toEither = \case
      Ok ast     -> Right ast
      Bad errMsg -> Left $ Errors.Error errMsg

-- | Read input from a given file name, or from stdin
-- if no file name is given.
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
    Right prog -> do
      hPutStrLn stderr "OK"
      putStrLn $ PrettyPrinter.prettyPrint 4 prog
      exitSuccess

    Left err -> do
      hPutStrLn stderr "ERROR"
      hPrint stderr err
      exitFailure
