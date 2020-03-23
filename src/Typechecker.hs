module Typechecker where

import Javalette.Abs
import Javalette.ErrM (Err (Ok, Bad))

typecheck :: Prog -> Err Prog
typecheck = return  -- TODO
