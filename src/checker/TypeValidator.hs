module Src.Checker.TypeValidator where

import Data.List ( sortOn )
import Control.Monad ( foldM )
import Control.Monad.Trans.Class ( MonadTrans ( lift ) )
import Control.Monad.Trans.Reader ( ask, ReaderT( runReaderT ) )

import Src.Synonym ( LineNumber, Scope, cGLOBAL )
import Src.Token
import Src.AST
import Src.Checker.DType ( DType (..) )
import Src.Checker.CheckMonad ( Check (..) )
import Src.Checker.ProcedureManager
import Src.Checker.VariableManager

type Tables   = (VariableTable, ProcedureTable)
type Operator = String

validateType :: AST -> (VariableTable, ProcedureTable) -> Check ()
validateType ast = runReaderT (visitProgram ast)

visitProgram :: AProgram -> ReaderT Tables Check ()
visitProgram (
        AProgram
            _
            block
            compoundStatement
    ) = do
        visitBlock block
        visitCompoundStatement compoundStatement cGLOBAL

visitBlock :: ABlock -> ReaderT Tables Check ()
visitBlock (
        ABlock
            _
            subprogramDeclarations
    ) = visitSubprogramDeclarations subprogramDeclarations

visitSubprogramDeclarations :: ASubprogramDeclarations -> ReaderT Tables Check ()
visitSubprogramDeclarations (
        ASubprogramDeclarations
            subprogramDeclarationList
    ) = mapM_ (\subproc -> visitSubprogramDeclaration subproc) subprogramDeclarationList

visitSubprogramDeclaration :: ASubprogramDeclaration -> ReaderT Tables Check ()
visitSubprogramDeclaration (
        ASubprogramDeclaration
            subprogramHead
            _
            compoundStatement
    ) = visitCompoundStatement compoundStatement scope
    where
        scope = visitSubprogramHead subprogramHead

visitSubprogramHead :: ASubprogramHead -> Scope
visitSubprogramHead (
        ASubprogramHead
            procedureName
            _
    ) = fst $ visitProcedureName procedureName

visitProcedureName :: AProcedureName -> (Scope, LineNumber)
visitProcedureName (
        AProcedureName
            identifier
    ) = visitIdentifier identifier

visitIdentifier :: AIdentifier -> (String, LineNumber)
visitIdentifier (
        AIdentifier
            token
    ) = (getSSymbol token, getSLineNumber token)

visitCompoundStatement :: ACompoundStatement -> Scope -> ReaderT Tables Check ()
visitCompoundStatement (
        ACompoundStatement
            statementSequence
    ) = visitStatementSequence statementSequence

visitStatementSequence :: AStatementSequence -> Scope -> ReaderT Tables Check ()
visitStatementSequence (
        AStatementSequence
            statement
            statementList
    ) scope = mapM_ (\stmt -> visitStatement stmt scope) (statement : statementList)

visitStatement :: AStatement -> Scope -> ReaderT Tables Check ()
visitStatement (
        ABasic
            basicStatement
    ) = visitBasicStatement basicStatement
visitStatement (
        ABranch
            ifStatement
    ) = visitIfStatement ifStatement
visitStatement (
        ARepeat
            whileStatement
    ) = visitWhileStatement whileStatement

visitBasicStatement :: ABasicStatemet -> Scope -> ReaderT Tables Check ()
visitBasicStatement (
        AAssignment
            assignmentStatement
    ) scope = visitAssignmentStatement assignmentStatement scope
visitBasicStatement (
        AProcedureCall
            procedureCallStatement
    ) scope = visitProcedureCallStatement procedureCallStatement scope
visitBasicStatement (
        AIO
         ioStatement
    ) scope = visitIOStatement ioStatement scope
visitBasicStatement (
        ACompound
            compoundStatement
    ) scope = visitCompoundStatement compoundStatement scope

visitAssignmentStatement :: AAssignmentStatement -> Scope -> ReaderT Tables Check ()
visitAssignmentStatement (
        AAssignmentStatement
            leftSide
            expression
    ) scope = do
        (dtypeL, lineNumber) <- visitLeftSide leftSide scope
        dtypeR <- visitExpression expression scope
        if dtypeL == dtypeR
        then return ()
        else lift $ SemanticError lineNumber

visitLeftSide :: ALeftSide -> Scope -> ReaderT Tables Check (DType, LineNumber)
visitLeftSide (
        ALeftSide
            variable
    ) = visitVariable variable

visitVariable :: AVariable -> Scope -> ReaderT Tables Check (DType, LineNumber)
visitVariable (
        APure
            pureVariable
    )  = visitPureVariable pureVariable 
visitVariable (
        AIndexed
            indexedVariable
    ) = visitIndexedVariable indexedVariable 

visitPureVariable :: APureVariable -> Scope -> ReaderT Tables Check (DType, LineNumber)
visitPureVariable (
        APureVariable
            variableName
    ) = visitVariableName variableName

visitVariableName :: AVariableName -> Scope -> ReaderT Tables Check (DType, LineNumber)
visitVariableName (
        AVariableName
            identifier
    ) scope = do
        (variableTable, _) <- ask
        let (name, lineNumber) = visitIdentifier identifier
        let lookupResult = lookupVarInfo scope name variableTable
        case lookupResult of 
            (Just info) -> return (getVarType info, lineNumber)
            Nothing     -> lift (SemanticError lineNumber)

visitIndexedVariable :: AIndexedVariable -> Scope -> ReaderT Tables Check (DType, LineNumber)
visitIndexedVariable (
        AIndexedVariable
            variableName
            index
    ) scope = do
        (arrayType, lineNumber) <- visitVariableName variableName scope
        dtype <- visitIndex index scope 
        if dtype == DInteger
        then 
            case arrayType of
                DIntegerArray -> return (DInteger, lineNumber)
                DCharArray    -> return (DChar,    lineNumber)
                DBooleanArray -> return (DBoolean, lineNumber)
        else lift $ SemanticError lineNumber

visitIndex :: AIndex -> Scope -> ReaderT Tables Check DType
visitIndex (
        AIndex
            expression
    ) = visitExpression expression

visitExpression :: AExpression -> Scope -> ReaderT Tables Check DType
visitExpression (
        AExpression
            simpleExpression
            Nothing
    ) scope = visitSimpleExpression simpleExpression scope
visitExpression (
        AExpression
            simpleExpression
            (Just relationalOperation)
    ) scope = do
        dtypeL <- visitSimpleExpression simpleExpression scope
        ((operator, lineNumber), dtypeR) <- visitRelationalOperation relationalOperation scope
        if dtypeL == dtypeR
        then return DBoolean
        else lift $ SemanticError lineNumber

visitSimpleExpression :: ASimpleExpression -> Scope -> ReaderT Tables Check DType
visitSimpleExpression (
        ASimpleExpression
            Nothing
            term
            additionalOperationList
    ) scope = do
        dtype <- visitTerm term scope
        expanded <- expandAdditionalOperations additionalOperationList scope
        lift $ foldM foldAdditionalOperations dtype expanded
visitSimpleExpression (
        ASimpleExpression
            (Just sign)
            term
            additionalOperationList
    ) scope = do
        let (operator, lineNumber) = visitSign sign
        dtype <- visitTerm term scope
        if dtype == DInteger
        then do
            expanded <- expandAdditionalOperations additionalOperationList scope
            lift $ foldM foldAdditionalOperations dtype expanded
        else
            lift $ SemanticError lineNumber

expandAdditionalOperations :: [AAdditionalOperation] -> Scope -> ReaderT Tables Check [((Operator, LineNumber), DType)]
expandAdditionalOperations [] _ = return []
expandAdditionalOperations (h:t) scope = do
    ((oprator, lineNumber), dtype) <- visitAdditionalOperation h scope
    tailResult <- expandAdditionalOperations t scope
    return $ ((oprator, lineNumber), dtype) : tailResult
foldAdditionalOperations :: DType -> ((Operator, LineNumber), DType) -> Check DType
foldAdditionalOperations dtypeL ((operator, lineNumber), dtypeR)
    | operator `elem` [cSPLUS, cSMINUS] =
        if dtypeL == DInteger && dtypeL == dtypeR
        then return dtypeL
        else SemanticError lineNumber
    | otherwise = -- 演算子は or で確定
        if dtypeL == DBoolean && dtypeL == dtypeR
        then return dtypeL
        else SemanticError lineNumber

visitTerm :: ATerm -> Scope -> ReaderT Tables Check DType
visitTerm (
        ATerm
            factor
            multiplicativeOperationList
    ) scope = do
        dtype <- visitFactor factor scope
        expanded <- expandMultiplicativeOperations multiplicativeOperationList scope
        lift $ foldM foldMultiplicativeOperations dtype expanded

expandMultiplicativeOperations :: [AMultiplicativeOperation] -> Scope -> ReaderT Tables Check [((Operator, LineNumber), DType)]
expandMultiplicativeOperations [] _ = return []
expandMultiplicativeOperations (h:t) scope = do
    ((oprator, lineNumber), dtype) <- visitMultiplicativeOperation h scope
    tailResult <- expandMultiplicativeOperations t scope
    return $ ((oprator, lineNumber), dtype) : tailResult
foldMultiplicativeOperations :: DType -> ((Operator, LineNumber), DType) -> Check DType
foldMultiplicativeOperations dtypeL ((operator, lineNumber), dtypeR)
    | operator `elem` [cSSTAR, cSDIVD, cSMOD] =
        if dtypeL == DInteger && dtypeL == dtypeR
        then return dtypeL
        else SemanticError lineNumber
    | otherwise = -- 演算子は and で確定
        if dtypeL == DBoolean && dtypeL == dtypeR
        then return dtypeL
        else SemanticError lineNumber

visitFactor :: AFactor -> Scope -> ReaderT Tables Check DType
visitFactor (
        AVariableReference
            variable
    ) scope = do
        (dtype, _) <- visitVariable variable scope
        return dtype
visitFactor (
        AConstantReference
            constant
    ) scope = return $ visitConstant constant
visitFactor (
        ARecursion
            expression
    ) scope = visitExpression expression scope
visitFactor (
        ANegation
            negationOperator
            factor
    ) scope = do
        let (_, lineNumber) = visitNegationOperator negationOperator
        dtype <- visitFactor factor scope
        if dtype == DBoolean
        then return dtype
        else lift $ SemanticError lineNumber

visitConstant :: AConstant -> DType
visitConstant (AIntegerLiteral   _) = DInteger
visitConstant (AStringLiteral    _) = DCharArray
visitConstant (ABooleanLiteral   _) = DBoolean
visitConstant (ACharacterLiteral _) = DChar

visitNegationOperator :: ANegationOperator -> (Operator, LineNumber)
visitNegationOperator (
        ANegationOperator
            token
    ) = (getSType token, getSLineNumber token)

visitMultiplicativeOperation :: AMultiplicativeOperation -> Scope -> ReaderT Tables Check ((Operator, LineNumber), DType)
visitMultiplicativeOperation (
        AMultiplicativeOperation
            multiplicativeOperator
            factor
    ) scope = do
        dtype <- visitFactor factor scope
        return ((operator, lineNumber), dtype)
    where
        (operator, lineNumber) = visitMultiplicativeOperator multiplicativeOperator

visitMultiplicativeOperator :: AMultiplicativeOperator -> (Operator, LineNumber)
visitMultiplicativeOperator (
        AMultiplicativeOperator
            token
    ) = (getSType token, getSLineNumber token)

visitAdditionalOperation :: AAdditionalOperation -> Scope -> ReaderT Tables Check ((Operator, LineNumber), DType)
visitAdditionalOperation (
        AAdditionalOperation
            additionalOperator
            term
    ) scope = do
        dtype <- visitTerm term scope
        return ((operator, lineNumber), dtype)
    where
        (operator, lineNumber) = visitAdditionalOperator additionalOperator

visitAdditionalOperator :: AAdditionalOperator -> (Operator, LineNumber)
visitAdditionalOperator (
        AAdditionalOperator
            token
    ) = (getSType token, getSLineNumber token)

visitSign :: ASign -> (Operator, LineNumber)
visitSign (
        ASign
            token
    ) = (getSType token, getSLineNumber token)

visitRelationalOperation :: ARelationalOperation -> Scope -> ReaderT Tables Check ((Operator, LineNumber), DType)
visitRelationalOperation (
        ARelationalOperation
            relationalOperator
            simpleExpression
    ) scope = do
        dtype <- visitSimpleExpression simpleExpression scope
        return ((operator, lineNumber), dtype)
    where
        (operator, lineNumber) = visitRelationalOperator relationalOperator

visitRelationalOperator :: ARelationalOperator -> (Operator, LineNumber)
visitRelationalOperator (
        ARelationalOperator
            token
    ) = (getSType token, getSLineNumber token)

visitProcedureCallStatement :: AProcedureCallStatement -> Scope -> ReaderT Tables Check ()
visitProcedureCallStatement (
        AProcedureCallStatement
            procedureName
            Nothing
    ) scope = do
        (_, procedureTable) <- ask
        let (calledName, calledLine) = visitProcedureName procedureName
        let lookupResult = lookupProcInfo calledName procedureTable
        case lookupResult of
            (Just info) -> return ()
            Nothing     -> lift $ SemanticError calledLine 
visitProcedureCallStatement (
        AProcedureCallStatement
            procedureName
            (Just expressionSequence)
    ) scope = do
        (_, procedureTable) <- ask
        let (calledName, calledLine) = visitProcedureName procedureName
        let lookupResult = lookupProcInfo calledName procedureTable
        case lookupResult of
            (Just info) -> do
                let requiredArgTypes = getProcArgTypes info
                argTypes <- visitExpressionSequence expressionSequence scope
                if argTypes == requiredArgTypes
                then return ()
                else lift $ SemanticError calledLine
            Nothing     -> lift $ SemanticError calledLine 

visitExpressionSequence :: AExpressionSequence -> Scope -> ReaderT Tables Check [DType]
visitExpressionSequence (
        AExpressionSequence
            expression
            expressionList
    ) scope = mapM (\expr -> visitExpression expr scope) (expression : expressionList)

visitIOStatement :: AIOStatement -> Scope -> ReaderT Tables Check ()
visitIOStatement (
        AInputStatement
            Nothing
    ) scope = return ()
visitIOStatement (
        AOutputStatement
            Nothing
    ) scope = return ()
visitIOStatement (
        AInputStatement
            (Just variableSequence)
    ) scope = do
        argList <- visitVariableSequecne variableSequence scope
        let filtered = filter (\(dtype, _) -> dtype `notElem` [DInteger, DChar, DCharArray]) argList
        if null filtered
        then return ()
        else
            let 
                -- 最も手前の型違反位置を返すため。
                errorLocation = head $ sortOn snd filtered
            in
                lift $ SemanticError $ snd errorLocation
visitIOStatement (
        AOutputStatement
            (Just expressionSequence)
    ) scope = do
        argList <- visitExpressionSequence expressionSequence scope
        let filtered = filter (\dtype -> dtype `notElem` [DInteger, DChar, DCharArray]) argList
        if null filtered
        then return ()
        else lift $ SemanticError $ show filtered

visitVariableSequecne :: AVariableSequence -> Scope -> ReaderT Tables Check [(DType, LineNumber)]
visitVariableSequecne (
        AVariableSequence
            variable
            variableList
    ) scope = mapM (\var -> visitVariable var scope) (variable : variableList)

visitIfStatement :: AIfStatement -> Scope -> ReaderT Tables Check ()
visitIfStatement (
        AIfStatement
            keyword
            expression
            compoundStatement
            elseStatement
    ) scope = do
        dtype <- visitExpression expression scope
        if dtype == DBoolean
        then do
            visitCompoundStatement compoundStatement scope
            visitElseStatement elseStatement scope
        else
            lift $ SemanticError $ visitIfKeyword keyword

visitIfKeyword :: AIfKeyWord -> LineNumber
visitIfKeyword (
        AIfKeyword
            token
    ) = getSLineNumber token

visitElseStatement :: AElseStatement -> Scope -> ReaderT Tables Check ()
visitElseStatement (
        AElseStatement 
            Nothing
    ) scope = return ()
visitElseStatement (
        AElseStatement
            (Just compoundStatement)
    ) scope = visitCompoundStatement compoundStatement scope

visitWhileStatement :: AWhileStatement -> Scope -> ReaderT Tables Check ()
visitWhileStatement (
        AWhileStatement
            keyword
            expression
            compoundStatement
    ) scope = do
        dtype <- visitExpression expression scope
        if dtype == DBoolean
        then do
            visitCompoundStatement compoundStatement scope
        else
            lift $ SemanticError $ visitWhileKeyword keyword

visitWhileKeyword :: AWhileKeyword -> LineNumber
visitWhileKeyword (
        AWhileKeyword
            token
    ) = getSLineNumber token