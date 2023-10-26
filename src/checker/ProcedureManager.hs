module Src.Checker.ProcedureManager where

import Data.Map.Strict as M
import Data.Set as S

import Src.AST
import Src.Checker.DType ( DType )
import Src.Checker.Checker ( Check (..) )

data Procedure = Procedure {
    getProcArgTypes :: [DType],
    getProcName :: String
}

type ProcedureTable = Set Procedure

constructProcedureTable :: AST -> Check ProcedureTable
constructProcedureTable = undefined