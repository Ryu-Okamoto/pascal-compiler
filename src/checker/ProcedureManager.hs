module Src.Checker.ProcedureManager ( ProcedureName, ProcedureTable, ProcedureInfo (..), constructProcedureTable ) where

import Data.Map.Strict as M

import Src.Synonym ( LineNumber )
import Src.Token ( Token (..) )
import Src.AST
import Src.Checker.DType ( DType (..) )
import Src.Checker.CheckMonad ( Check (..) )

data ProcedureInfo = ProcedureInfo {
    getProcArgTypes    :: [DType],
    getProcDefinedLine :: LineNumber
} deriving ( Show )

type ProcedureName  = String
type ProcedureTable = Map ProcedureName ProcedureInfo

{-
    実装方針：
     - AST 要素のうち必要なとこだけを visit
       - 重複宣言がある場合はその行番号とともに SemanticError
       - そうでない場合は ProcedureTable を返す
        - ProcedureTable は (ProcedureName, ProcedureInfo) の Map
-}

constructProcedureTable :: AST -> Check ProcedureTable
constructProcedureTable = visitProgram

visitProgram :: AProgram -> Check ProcedureTable
visitProgram (
        AProgram
            _
            block
            _
    ) = visitBlock block

visitBlock :: ABlock -> Check ProcedureTable
visitBlock (
        ABlock
            _
            subprogramDeclarations
    ) = visitSubprogramDeclarations subprogramDeclarations

visitSubprogramDeclarations :: ASubprogramDeclarations -> Check ProcedureTable
visitSubprogramDeclarations (
        ASubprogramDeclarations
            subprogramDeclarationList
    ) = let
            -- 前の方から順に検査するために。createProcedureTable の実装が末尾再帰だから。
            reversed = reverse subprogramDeclarationList
        in
            createProcedureTable reversed
    where
        createProcedureTable :: [ASubprogramDeclaration] -> Check ProcedureTable
        createProcedureTable [] = return M.empty
        createProcedureTable (h:t) = do
            let newTable = visitSubprogramDeclaration h
            createdTable <- createProcedureTable t
            let duplicatingTable = createdTable `M.intersection` newTable
            if M.null duplicatingTable
            then 
                return $ newTable `M.union` createdTable
            else do
                let duplicating = M.elemAt 0 duplicatingTable
                SemanticError $ getProcDefinedLine $ snd duplicating   

visitSubprogramDeclaration :: ASubprogramDeclaration -> ProcedureTable
visitSubprogramDeclaration (
        ASubprogramDeclaration
            subprogramHead
            _
            _
    ) = visitSubprogramHead subprogramHead

visitSubprogramHead :: ASubprogramHead -> ProcedureTable
visitSubprogramHead (
        ASubprogramHead
            procedureName
            parameter
    ) = M.fromList [(name, info)]
    where
        (name, lineNumber) = visitProcedureName procedureName
        argTypes = visitParameter parameter
        info = ProcedureInfo argTypes lineNumber

visitProcedureName :: AProcedureName -> (ProcedureName, LineNumber)
visitProcedureName (
    AProcedureName
        identifier
    ) = visitIdentifier identifier

visitIdentifier :: AIdentifier -> (ProcedureName, LineNumber)
visitIdentifier (
        AIdentifier
            token
    ) = (getSSymbol token, getSLineNumber token)

visitParameter :: AParameter -> [DType]
visitParameter (
        AParameter
            Nothing
    ) = []
visitParameter (
        AParameter
            (Just parameterSequence)
    ) = visitParameterSequence parameterSequence

visitParameterSequence :: AParameterSequence -> [DType]
visitParameterSequence (
        AParameterSequence
            parameterSequence'
            parameterSequence'List
    ) = createDTypeList $ parameterSequence' : parameterSequence'List
    where
        createDTypeList :: [AParameterSequence'] -> [DType]
        createDTypeList [] = []
        createDTypeList (h:t) = replicate count dtype ++ createDTypeList t
            where
                (dtype, count) = visitParameterSequence' h
        
visitParameterSequence' :: AParameterSequence' -> (DType, Int)
visitParameterSequence' (
        AParameterSequence'
            parameterNameSequence
            standardType
    ) = (visitStandardType standardType, visitParameterNameSequence parameterNameSequence)

visitParameterNameSequence :: AParameterNameSequence -> Int
visitParameterNameSequence (
        AParameterNameSequence
            parameterName
            parameterNameList
    ) = length $ parameterName : parameterNameList

visitStandardType :: AStandardType -> DType
visitStandardType (
        AStandardType
            token
    ) = case getSType token of
        "SINTEGER" -> DInteger
        "SCHAR"    -> DChar
        "SBOOLEAN" -> DBoolean