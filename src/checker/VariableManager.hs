module Src.Checker.VariableManager ( VariableName, VariableTable, VariableTableMap, VariableInfo (..), constructVariableTableMap, lookupVarInfo ) where

import Data.Map.Strict as M

import Src.Synonym ( LineNumber, Scope, cGLOBAL )
import Src.Token ( Token (..) )
import Src.AST
import Src.Checker.DType ( DType (..) )
import Src.Checker.CheckMonad ( Check (..) )

data VariableInfo = VariableInfo {
    getVarType        :: DType,
    getVarDefinedLine :: LineNumber
} deriving ( Show )

type VariableName     = String
type VariableTable    = Map VariableName VariableInfo
type VariableTableMap = Map Scope VariableTable

lookupVarInfo :: Scope -> VariableName -> VariableTableMap -> Maybe VariableInfo
lookupVarInfo scope name map = do
    localTable <- M.lookup scope map
    if name `M.member` localTable
    then M.lookup name localTable
    else do
        globalTable <- M.lookup cGLOBAL map
        M.lookup name globalTable

{-
    実装方針：
     - AST 要素のうち必要なとこだけを visit
       - 重複宣言がある場合はその行番号とともに SemanticError
       - そうでない場合は VariableTableMap を返す
        - VariableTableMap は (Scope, VariableTable) の Map
         - VariableTable は (VariableName, VariableInfo) の Map
-}

constructVariableTableMap :: AST -> Check VariableTableMap
constructVariableTableMap = visitProgram

visitProgram :: AProgram -> Check VariableTableMap
visitProgram (
        AProgram
            _
            block
            _
    ) = visitBlock block

visitBlock :: ABlock -> Check VariableTableMap
visitBlock (
        ABlock
            variableDeclaration
            subprogramDeclarations
    ) = do
        globalTable <- visitVariableDeclaration variableDeclaration
        localTableMap <- visitSubprogramDeclarations subprogramDeclarations
        return $ M.insert cGLOBAL globalTable localTableMap

visitVariableDeclaration :: AVariableDeclaration -> Check VariableTable
visitVariableDeclaration (
        AVariableDeclaration
            Nothing
    ) = return M.empty
visitVariableDeclaration (
        AVariableDeclaration
            (Just variableDeclarationSequence)
    ) = visitVariableDeclarationSequence variableDeclarationSequence

visitVariableDeclarationSequence :: AVariableDeclarationSequence -> Check VariableTable
visitVariableDeclarationSequence (
        AVariableDeclarationSequence
            variableDeclarationSequence'
            variableDeclarationSequence'List
    ) = let
            -- 前の方から順に検査するために。createVariableTable の実装が末尾再帰だから。
            reversed = reverse $ variableDeclarationSequence' : variableDeclarationSequence'List
        in
            createVariableTable reversed
    where
        createVariableTable :: [AVariableDeclarationSequence'] -> Check VariableTable
        createVariableTable [] = return M.empty
        createVariableTable (h:t) = do
            newTable <- visitVariableDeclarationSequence' h
            createdTable <- createVariableTable t
            let duplicatingTable = newTable `M.intersection` createdTable
            if M.null duplicatingTable
            then 
                return $ newTable `M.union` createdTable
            else do
                let duplicating = M.elemAt 0 duplicatingTable
                SemanticError $ getVarDefinedLine $ snd duplicating

visitVariableDeclarationSequence' :: AVariableDeclarationSequence' -> Check VariableTable
visitVariableDeclarationSequence' (
        AVariableDeclarationSequence'
            variableNameSequence
            type_
    ) = do
        variableNameList <- visitVariableNameSequence variableNameSequence
        let dtype = visitType type_
        return $ 
            M.fromList $
                Prelude.map (\(name, line) -> (name, VariableInfo dtype line)) variableNameList

visitVariableNameSequence :: AVariableNameSequence -> Check [(VariableName, LineNumber)]
visitVariableNameSequence (
        AVariableNameSequence
            variableName
            variableNameList
    ) = let
            -- 前の方から順に検査するために。createVariableNameList の実装が末尾再帰だから。
            reversed = reverse $ variableName : variableNameList
        in
            createVariableNameList reversed
    where
        createVariableNameList :: [AVariableName] -> Check [(VariableName, LineNumber)]
        createVariableNameList [] = return []
        createVariableNameList (h:t) = do
            let newName = visitVariableName h
            definedNameList <- createVariableNameList t
            if newName `notElem` definedNameList
            then return $ newName : definedNameList
            else SemanticError $ snd newName

visitVariableName :: AVariableName -> (VariableName, LineNumber)
visitVariableName (
        AVariableName
            identifier
    ) = visitIdentifier identifier

visitIdentifier :: AIdentifier -> (VariableName, LineNumber)
visitIdentifier (
        AIdentifier
            token
    ) = (getSSymbol token, getSLineNumber token)

visitType :: AType -> DType
visitType (
        AStandard
            standardType
    ) = visitStandardType standardType
visitType (
        AArray
            arrayType
    ) = visitArrayType arrayType

visitStandardType :: AStandardType -> DType
visitStandardType (
        AStandardType
            token
    ) = case getSType token of
        "SINTEGER" -> DInteger
        "SCHAR"    -> DChar
        "SBOOLEAN" -> DBoolean

visitArrayType :: AArrayType -> DType
visitArrayType (
        AArrayType
            _
            _
            standardType
    ) = case dtype of
            DInteger -> DIntegerArray
            DChar    -> DCharArray
            DBoolean -> DBooleanArray
    where
        dtype = visitStandardType standardType

visitSubprogramDeclarations :: ASubprogramDeclarations -> Check VariableTableMap
visitSubprogramDeclarations (
        ASubprogramDeclarations
            subprogramDeclarationList
    ) = let
            -- 前の方から順に検査するために。createVariableTableMap の実装が末尾再帰だから。
            reversed = reverse subprogramDeclarationList
        in
            createVariableTableMap reversed
    where
        createVariableTableMap :: [ASubprogramDeclaration] -> Check VariableTableMap
        createVariableTableMap [] = return M.empty
        createVariableTableMap (h:t) = do
            newMap <- visitSubprogramDeclaration h
            createdMap <- createVariableTableMap t
            return $ newMap `M.union` createdMap

visitSubprogramDeclaration :: ASubprogramDeclaration -> Check VariableTableMap
visitSubprogramDeclaration (
        ASubprogramDeclaration
            subprogramHead
            variableDeclaration
            _
    ) = do
        (scope, parameterTable) <- visitSubprogramHead subprogramHead
        variableTable <- visitVariableDeclaration variableDeclaration
        let duplicatingTable = variableTable `M.intersection` parameterTable
        if M.null duplicatingTable
        then
            return $ M.fromList [(scope, parameterTable `M.union` variableTable)]
        else do
            let duplicating = M.elemAt 0 duplicatingTable
            SemanticError $ getVarDefinedLine $ snd duplicating

visitSubprogramHead :: ASubprogramHead -> Check (Scope, VariableTable)
visitSubprogramHead (
        ASubprogramHead
            procedureName
            parameter
    ) = do
        let scope = visitProcedureName procedureName
        parameterTable <- visitParameter parameter
        return (scope, parameterTable)

visitProcedureName :: AProcedureName -> Scope
visitProcedureName (
        AProcedureName
            identifier
    ) = fst $ visitIdentifier identifier

visitParameter :: AParameter -> Check VariableTable
visitParameter (
        AParameter
            Nothing
    ) = return M.empty
visitParameter (
        AParameter
            (Just parameterSequence)
    ) = visitParameterSequence parameterSequence

visitParameterSequence :: AParameterSequence -> Check VariableTable
visitParameterSequence (
        AParameterSequence
            parameterSequence'
            parameterSequence'List
    ) = let
            -- 前の方から順に検査するために。createParameterTable の実装が末尾再帰だから。
            reversed = reverse $ parameterSequence' : parameterSequence'List
        in
            createParameterTable reversed
    where
        createParameterTable :: [AParameterSequence'] -> Check VariableTable
        createParameterTable [] = return M.empty
        createParameterTable (h:t) = do
            newTable <- visitParameterSequence' h
            createdTable <- createParameterTable t
            let duplicatingTable = newTable `M.intersection` createdTable
            if M.null duplicatingTable
            then 
                return $ newTable `M.union` createdTable
            else do
                let duplicating = M.elemAt 0 duplicatingTable
                SemanticError $ getVarDefinedLine $ snd duplicating

visitParameterSequence' :: AParameterSequence' -> Check VariableTable
visitParameterSequence' (
        AParameterSequence'
            parameterNameSequence
            standardType
    ) = do
        parameterNameList <- visitParameterNameSequence parameterNameSequence
        let dtype = visitStandardType standardType
        return $ 
            M.fromList $
                Prelude.map (\(name, line) -> (name, VariableInfo dtype line)) parameterNameList

visitParameterNameSequence :: AParameterNameSequence -> Check [(VariableName, LineNumber)]
visitParameterNameSequence (
        AParameterNameSequence
            parameterName
            parameterNameList
    ) = let
            -- 前の方から順に検査するために。createParameterNameList の実装が末尾再帰だから。
            reversed = reverse $ parameterName : parameterNameList
        in
            createParameterNameList reversed
    where
        createParameterNameList :: [AParameterName] -> Check [(VariableName, LineNumber)]
        createParameterNameList [] = return []
        createParameterNameList (h:t) = do
            let newName = visitParameterName h
            definedNameList <- createParameterNameList t
            if newName `notElem` definedNameList
            then return $ newName : definedNameList
            else SemanticError $ snd newName

visitParameterName :: AParameterName -> (VariableName, LineNumber)
visitParameterName (
        AParameterName
            identifier
    ) = visitIdentifier identifier