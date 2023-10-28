module Src.Checker.VariableManager where

import Data.Map.Strict as M
import Data.Set as S

import Src.Synonym ( Scope, LineNumber, cGLOBAL )
import Src.Token ( Token (..) )
import Src.AST
import Src.Checker.DType ( DType (..) )
import Src.Checker.Checker ( Check (..) )

data Variable = Variable {
    getVarType :: DType,
    getVarName :: String,
    getVarDefinedLineNumber :: LineNumber
} deriving ( Show )
instance Eq Variable where
    (==) :: Variable -> Variable -> Bool
    (==) (Variable _ name1 _) (Variable _ name2 _) = name1 == name2
instance Ord Variable where
    (<=) :: Variable -> Variable -> Bool
    (<=) (Variable _ name1 _) (Variable _ name2 _) = name1 <= name2

type VariableTable = Map Scope (Set Variable)

{-
    実装方針：
     - AST 要素のうち必要なとこだけを visit
       - 重複宣言がある場合はその行番号とともに SemanticError
       - そうでない場合は (Scope, VariableTable) の Map を返す
-}

constructVariableTable :: AST -> Check VariableTable
constructVariableTable = visitProgram

visitProgram :: AProgram -> Check VariableTable
visitProgram (
    AProgram
        _
        block
        _
    ) = visitBlock block

visitBlock :: ABlock -> Check VariableTable
visitBlock (
    ABlock
        variableDeclaration
        subprogramDeclarations
    ) = do
        globalVariableSet <- visitVariableDeclaration variableDeclaration
        let globalTable = M.fromList [(cGLOBAL, globalVariableSet)]
        localTables <- visitSubprogramDeclarations subprogramDeclarations
        return $ M.unions $ S.insert globalTable localTables

visitVariableDeclaration :: AVariableDeclaration -> Check (Set Variable)
visitVariableDeclaration (
    AVariableDeclaration
        Nothing
    ) = return S.empty
visitVariableDeclaration (
    AVariableDeclaration
        (Just variableDeclarationSequence)
    ) = visitVariableDeclarationSequence variableDeclarationSequence

visitVariableDeclarationSequence :: AVariableDeclarationSequence -> Check (Set Variable)
visitVariableDeclarationSequence (
    AVariableDeclarationSequence
        variableDeclarationSequence'
        variableDeclarationSequence'List
    ) = let
            -- 前の方から順に検査するために。createVariableSet の実装が末尾再帰だから。
            reversed = reverse $ variableDeclarationSequence' : variableDeclarationSequence'List
        in
        createVariableSet reversed
    where
        createVariableSet :: [AVariableDeclarationSequence'] -> Check (Set Variable)
        createVariableSet [] = return S.empty
        createVariableSet (h:t) = do
            newVariableSet <- visitVariableDeclarationSequence' h
            definedVariableSet <- createVariableSet t
            let duplicatingNamedVariableSet = newVariableSet `S.intersection` definedVariableSet
            if duplicatingNamedVariableSet /= S.empty
            then
                let
                    duplicatingNamedVariabale = S.elemAt 0 duplicatingNamedVariableSet
                in
                SemanticError $ getVarDefinedLineNumber duplicatingNamedVariabale
            else
                return $ newVariableSet `S.union` definedVariableSet

visitVariableDeclarationSequence' :: AVariableDeclarationSequence' -> Check (Set Variable)
visitVariableDeclarationSequence' (
    AVariableDeclarationSequence'
        variableNameSequence
        type_
    ) = do
        variableNameSet <- visitVariableNameSequence variableNameSequence
        let dtype = visitType type_
        return $ S.map (uncurry (Variable dtype)) variableNameSet

visitVariableNameSequence :: AVariableNameSequence -> Check (Set (String, LineNumber))
visitVariableNameSequence (
    AVariableNameSequence
        variableName
        variableNameList
    ) = let
            -- 前の方から順に検査するために。createVariableNameSet の実装が末尾再帰だから。
            reversed = reverse $ variableName : variableNameList
        in
        createVariableNameSet reversed
    where
        createVariableNameSet :: [AVariableName] -> Check (Set (String, LineNumber))
        createVariableNameSet [] = return S.empty
        createVariableNameSet (h:t) = do
            let newVariableName = visitVariableName h
            definedVariableNameSet <- createVariableNameSet t
            if newVariableName `S.member` definedVariableNameSet
            then
                SemanticError $ snd newVariableName
            else
                return $ S.insert newVariableName definedVariableNameSet

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
    ) =
    let
        type_ = visitStandardType standardType
    in
        case type_ of
            DInteger -> DIntegerArray
            DChar    -> DCharArray
            DBoolean -> DBooleanArray

visitVariableName :: AVariableName -> (String, LineNumber)
visitVariableName (
    AVariableName
        identifier
    ) = visitIdentifier identifier

visitIdentifier :: AIdentifier -> (String, LineNumber)
visitIdentifier (
    AIdentifier
        token
    ) = (getSSymbol token, getSLineNumber token)

visitSubprogramDeclarations :: ASubprogramDeclarations -> Check (Set VariableTable)
visitSubprogramDeclarations (
    ASubprogramDeclarations
        subprogramDeclarationList
    ) = createLocalTables subprogramDeclarationList
    where
        createLocalTables :: [ASubprogramDeclaration] -> Check (Set VariableTable)
        createLocalTables [] = return S.empty
        createLocalTables (h:t) = do
            localTable <- visitSubprogramDeclaration h
            localTables <- createLocalTables t
            return $ S.insert localTable localTables

visitSubprogramDeclaration :: ASubprogramDeclaration -> Check VariableTable
visitSubprogramDeclaration (
    ASubprogramDeclaration
        subprogramHead
        variableDeclaration
        _
    ) = do
        (scope, parameterSet) <- visitSubprogramHead subprogramHead
        localVariableSet <- visitVariableDeclaration variableDeclaration
        let duplicatingNamedVariableSet = parameterSet `S.intersection` localVariableSet
        if duplicatingNamedVariableSet /= S.empty
        then
            let
                duplicatingNamedVariable = S.elemAt 0 duplicatingNamedVariableSet
            in
            SemanticError $ getVarDefinedLineNumber duplicatingNamedVariable
        else
            return $ M.fromList [(scope, parameterSet `S.union` localVariableSet)]

visitSubprogramHead :: ASubprogramHead -> Check (Scope, Set Variable)
visitSubprogramHead (
    ASubprogramHead
        procedureName
        parameter
    ) = do
        let scope = visitProcedureName procedureName
        parameterSet <- visitParameter parameter
        return (scope, parameterSet)

visitProcedureName :: AProcedureName -> Scope
visitProcedureName (
    AProcedureName
        identifier
    ) = fst $ visitIdentifier identifier

visitParameter :: AParameter -> Check (Set Variable)
visitParameter (
    AParameter
        Nothing
    ) = return S.empty
visitParameter (
    AParameter
        (Just parameterSequence)
    ) = visitParameterSequence parameterSequence

visitParameterSequence :: AParameterSequence -> Check (Set Variable)
visitParameterSequence (
    AParameterSequence
        parameterSequence'
        parameterSequence'List
    ) = let
            -- 前の方から順に検査するために。createParameterSet の実装が末尾再帰だから。
            reversed = reverse $ parameterSequence' : parameterSequence'List
        in
        createParameterSet reversed
    where
        createParameterSet :: [AParameterSequence'] -> Check (Set Variable)
        createParameterSet [] = return S.empty
        createParameterSet (h:t) = do
            newParameterSet <- visitParameterSequence' h
            definedParameterSet <- createParameterSet t
            let duplicatingNamedParameterSet = newParameterSet `S.intersection` definedParameterSet
            if duplicatingNamedParameterSet /= S.empty
            then
                let
                    duplicatingNamedParameter = S.elemAt 0 duplicatingNamedParameterSet
                in
                SemanticError $ getVarDefinedLineNumber duplicatingNamedParameter
            else
                return $ newParameterSet `S.union` definedParameterSet

visitParameterSequence' :: AParameterSequence' -> Check (Set Variable)
visitParameterSequence' (
    AParameterSequence'
        parameterNameSequence
        standardType
    ) = do
        parameterNameSet <- visitParameterNameSequence parameterNameSequence
        let dtype = visitStandardType standardType
        return $ S.map (uncurry (Variable dtype)) parameterNameSet

visitParameterNameSequence :: AParameterNameSequence -> Check (Set (String, LineNumber))
visitParameterNameSequence (
    AParameterNameSequence
        parameterName
        parameterNameList
    ) = let
            -- 前の方から順に検査するために。createParameterNameSet の実装が末尾再帰だから。
            reversed = reverse $ parameterName : parameterNameList
        in
        createParameterNameSet reversed
    where
        createParameterNameSet :: [AParameterName] -> Check (Set (String, LineNumber))
        createParameterNameSet [] = return S.empty
        createParameterNameSet (h:t) = do
            let newParameterName = visitParamerterName h
            definedParameterNameSet <- createParameterNameSet t
            if newParameterName `S.member` definedParameterNameSet
            then
                SemanticError $ snd newParameterName
            else
                return $ S.insert newParameterName definedParameterNameSet

visitParamerterName :: AParameterName -> (String, LineNumber)
visitParamerterName (
    AParameterName
        identifier
    ) = visitIdentifier identifier