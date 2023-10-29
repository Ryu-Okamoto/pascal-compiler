module Src.Checker.Checker ( run ) where

import Src.Synonym ( LineNumber )
import Src.AST
import Src.Checker.CheckMonad ( Check (..) )
import Src.Checker.VariableManager
import Src.Checker.ProcedureManager
import Src.Checker.TypeValidator

{-
    実装方針：
     1. 変数表を作る。同スコープに重複宣言があれば SemanticError
     2. 手続き表を作る。重複宣言があれば SemanticError
     3. 型検査
-}

run :: AST -> Check ()
run ast = do
    variableTableMap <- constructVariableTableMap ast
    procedureTable <- constructProcedureTable ast
    validateType (variableTableMap, procedureTable) ast