{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (getContents, hPrint, hPutStrLn, stderr, stdin)

import Javalette.Abs (Prog)
import Javalette.ErrM (Err (Ok, Bad))
import Javalette.Par (pProg, myLexer)

import qualified GHC.Stack as Stack

import qualified Frontend.AlphaRename as AlphaRename
import qualified Frontend.Desugar as Desugar
import qualified Frontend.Errors as Errors
import qualified Frontend.OptimizeAST as OptimizeAST
import qualified Frontend.PrettyPrinter as PrettyPrinter
import qualified Frontend.TypeChecker as TypeChecker


run :: String -> Either Errors.Error Prog
run code = do
  -- Lex
  let tokens = myLexer code

  -- Parse
  ast <- toEither $ pProg tokens

  -- Type check. We perform type checking before desugaring and
  -- other preprocessing, as we want errors to be as helpful as
  -- possible. However, we perform type checking twice; once
  -- to throw user errors, and then one later to annotate and
  -- to ensure that other preprocessing has not introduced errors.
  TypeChecker.typeCheck ast

  -- Alpha-rename. Note that we can work under the assumption that
  -- we do not have type errors due to the previous type check.
  let alphaRenamedAst = AlphaRename.alphaRename ast

  -- Desugar; assumes that alpha-renaming has been performed.
  let desugaredAst = Desugar.desugar alphaRenamedAst

  -- Optimize the AST simplifiying and rewriting some constructs.
  let optimizedAst = OptimizeAST.optimizeAst desugaredAst

  -- Type check again and annotate. A type error here indeicates
  -- a bug in the compiler, not a user error.
  let checkedAst2 = either compilerErr id (TypeChecker.typeCheck optimizedAst)

  return checkedAst2

  where
    toEither :: Err Prog -> Either Errors.Error Prog
    toEither = \case
      Ok ast     -> Right ast
      Bad errMsg -> Left $ Errors.Error errMsg

    compilerErr :: Stack.HasCallStack => Errors.Error -> a
    compilerErr err = error $ Errors.compilerErrMsg ++ mconcat
      [ "\n"
      , "The Error thrown was:\n"
      , show err
      ]

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
