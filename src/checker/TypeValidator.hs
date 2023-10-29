module Src.Checker.TypeValidator ( validateType ) where

import Src.AST
import Src.Checker.VariableManager
import Src.Checker.ProcedureManager
import Src.Checker.CheckMonad ( Check (..) )

validateType :: (VariableTableMap, ProcedureTable) -> AST -> Check ()
validateType _ _ = return ()