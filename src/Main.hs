{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Control.Monad.Except as E
import           System.Environment (getArgs)
import           System.Exit (exitFailure)

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

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- Only supports a single file currently
    [file] -> run <$> readFile file >>= \case
      Right a  -> putStrLn "OK."    >> print a
      Left err -> putStrLn "ERROR." >> error (show err)

    _      -> do
      putStrLn "> Run on a Javalette source file"
      exitFailure
