module Src.Checker.VariableManager where

import Data.Map.Strict as M
import Data.Set as S

import Src.Synonym ( Scope, LineNumber )
import Src.AST
import Src.Checker.DType ( DType )
import Src.Checker.Checker ( Check (..) )

data Variable = Variable {
    getVarType :: DType,
    getVarName :: String
}

type VariableTable = Map Scope (Set Variable)

constructVariableTable :: AST -> Check VariableTable
constructVariableTable = undefined