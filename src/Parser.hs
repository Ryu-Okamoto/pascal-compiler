{-# LANGUAGE InstanceSigs #-}

module Parser where

import AST
import Token
import Utils

data Parse a = SyntaxError LineNumber | Parse a deriving (Show)
instance Functor Parse where
    fmap :: (a -> b) -> Parse a -> Parse b
    fmap f (SyntaxError lineNumber) = SyntaxError lineNumber
    fmap f (Parse x) = Parse (f x)
instance Applicative Parse where
    pure :: a -> Parse a
    pure = Parse
    (<*>) :: Parse (a -> b) -> Parse a -> Parse b
    (<*>) _ (SyntaxError lineNumber) = SyntaxError lineNumber
    (<*>) (SyntaxError lineNumber) _ = SyntaxError lineNumber
    (<*>) (Parse f) (Parse x) = Parse (f x)
instance Monad Parse where
    (>>=) :: Parse a -> (a -> Parse b) -> Parse b
    (>>=) (SyntaxError lineNumber) _ = SyntaxError lineNumber
    (>>=) (Parse x) f = f x


parse :: InputFilePath -> IO ()
parse inputFilePath = do
    lines <- lines <$> readFile inputFilePath
    let tokens = map lineToToken lines
        result = parseProgram tokens
    case result of
      Parse x -> putStrLn "OK"
      SyntaxError n -> putStrLn $ "Syntax error: line " ++ show n

parseProgram :: [TokenInfo] -> Parse (Program, [TokenInfo])
parseProgram (h:t)
    | getTokenType h == "SPROGRAM" = do
        (programName, restTokens) <- parseProgramName t
        if null restTokens then SyntaxError $ getLineNumber h
        else
            let h1:t1 = restTokens in
            if getTokenType h1 == "SSEMICOLON" then do
                (block, restTokens1) <- parseBlock t1
                (compoundStatement, restTokens2) <- parseCompoundStatement restTokens1
                if null restTokens2 then SyntaxError $ getLineNumber h1
                else
                    let h2:t2 = restTokens2 in
                    if getTokenType h2 == "SDOT" then return (Program programName block compoundStatement, t2)
                    else SyntaxError $ getLineNumber h2
            else SyntaxError $ getLineNumber h1
    | otherwise = SyntaxError $ getLineNumber h
parseProgram [] = SyntaxError (-1)

parseProgramName :: [TokenInfo] -> Parse (ProgramName, [TokenInfo])
parseProgramName tokens = do
    (identifier, restTokens) <- parseIdentifier tokens
    return (ProgramName identifier, restTokens)

parseBlock :: [TokenInfo] -> Parse (Block, [TokenInfo])
parseBlock tokens = do
    (variableDeclaration, restTokens) <- parseVariableDeclaration tokens
    (subprogramDeclarations, restTokens1) <- parseSubprogramDeclarations restTokens
    return (Block variableDeclaration subprogramDeclarations, restTokens1)

parseVariableDeclaration :: [TokenInfo] -> Parse (VariableDeclaration, [TokenInfo])
parseVariableDeclaration (h:t)
    | getTokenType h == "SVAR" = do
        (variableDeclarationSequence, restTokens) <- parseVariableDeclarationSequence t
        return (VariableDeclaration (Just variableDeclarationSequence), restTokens)
    | otherwise = return (VariableDeclaration Nothing, h:t)
parseVariableDeclaration [] = SyntaxError (-1)

parseVariableDeclarationSequence :: [TokenInfo] -> Parse (VariableDeclarationSequence, [TokenInfo])
parseVariableDeclarationSequence tokens = do
    (variableNameSequence', restTokens) <- parseVariableDeclarationSequence' tokens
    (variableNameSequence'List, restTokens1) <- parseVariableDeclarationSequence'List restTokens
    return (VariableDeclarationSequence variableNameSequence' variableNameSequence'List, restTokens1)
    where
        parseVariableDeclarationSequence'List :: [TokenInfo] -> Parse ([VariableDeclarationSequence'], [TokenInfo])
        parseVariableDeclarationSequence'List (h:t)
            | getTokenType h == "SIDENTIFIER" = do
                (variableDeclarationSequence', restTokens) <- parseVariableDeclarationSequence' (h:t)
                (variableDeclarationSequence'List, restTokens1) <- parseVariableDeclarationSequence'List restTokens
                return (variableDeclarationSequence' : variableDeclarationSequence'List, restTokens1)
            | otherwise = return ([], h:t)
        parseVariableDeclarationSequence'List [] = return ([], [])

parseVariableDeclarationSequence' :: [TokenInfo] -> Parse (VariableDeclarationSequence', [TokenInfo])
parseVariableDeclarationSequence' (h:t)
    | getTokenType h == "SIDENTIFIER" = do
        (variableNameSequence, restTokens) <- parseVariableNameSequence (h:t)
        if null restTokens then SyntaxError $ getLineNumber h
        else
            let h1:t1 = restTokens in
            if getTokenType h1 == "SCOLON" then do
                (type_, restTokens1) <- parseType t1
                if null restTokens1 then SyntaxError $ getLineNumber h1
                else
                    let h2:t2 = restTokens1 in
                    if getTokenType h2 == "SSEMICOLON" then return (VariableDeclarationSequence' variableNameSequence type_, t2)
                    else SyntaxError $ getLineNumber h2
            else SyntaxError $ getLineNumber h1
    | otherwise = SyntaxError $ getLineNumber h
parseVariableDeclarationSequence' [] = SyntaxError (-1)

parseVariableNameSequence :: [TokenInfo] -> Parse (VariableNameSequence, [TokenInfo])
parseVariableNameSequence tokens = do
    (variableName, restTokens) <- parseVariableName tokens
    (variableNameList, restTokens1) <- parseVariableNameList restTokens
    return (VariableNameSequence variableName variableNameList, restTokens1)
    where
        parseVariableNameList :: [TokenInfo] -> Parse ([VariableName], [TokenInfo])
        parseVariableNameList (h:t)
            | getTokenType h == "SCOMMA" = do
                (variableName, restTokens) <- parseVariableName t
                (variableNameList, restTokens1) <- parseVariableNameList restTokens
                return (variableName : variableNameList, restTokens1)
            | otherwise = return ([], h:t)
        parseVariableNameList [] = return ([], [])

parseVariableName :: [TokenInfo] -> Parse (VariableName, [TokenInfo])
parseVariableName tokens = do
    (identifier, restTokens) <- parseIdentifier tokens
    return (VariableName identifier, restTokens)

parseType :: [TokenInfo] -> Parse (Type, [TokenInfo])
parseType (h:t)
    | getTokenType h `elem` ["SINTEGER", "SCHAR", "SBOOLEAN"] = do
        (standardType, restTokens) <- parseStandardType (h:t)
        return (Standard standardType, restTokens)
    | getTokenType h == "SARRAY" = do
        (arrayType, restTokens) <- parseArrayType (h:t)
        return (Array arrayType, restTokens)
    | otherwise = SyntaxError $ getLineNumber h
parseType [] = SyntaxError (-1)

parseStandardType :: [TokenInfo] -> Parse (StandardType, [TokenInfo])
parseStandardType (h:t)
    | getTokenType h `elem` ["SINTEGER", "SCHAR", "SBOOLEAN"] = return (StandardType h, t)
    | otherwise = SyntaxError $ getLineNumber h
parseStandardType [] = SyntaxError (-1)

parseArrayType :: [TokenInfo] -> Parse (ArrayType, [TokenInfo])
parseArrayType (h:t)
    | getTokenType h == "SARRAY" =
        if null t then SyntaxError $ getLineNumber h
        else
            let h1:t1 = t in
            if getTokenType h1 == "SLBRACKET" then do
                (minimumIndex, restTokens) <- parseMinimumIndex t1
                if null restTokens then SyntaxError $ getLineNumber h1
                else
                    let h2:t2 = restTokens in
                    if getTokenType h2 == "SRANGE" then do
                        (maximumIndex, restTokens1) <- parseMaximumIndex t2
                        if null restTokens1 then SyntaxError $ getLineNumber h2
                        else
                            let h3:t3 = restTokens1 in
                            if getTokenType h3 == "SRBRACKET" then
                                if null t3 then SyntaxError $ getLineNumber h3
                                else
                                    let h4:t4 = t3 in
                                    if getTokenType h4 == "SOF" then do
                                        (standardType, restTokens2) <- parseStandardType t4
                                        return (ArrayType minimumIndex maximumIndex standardType, restTokens2)
                                    else SyntaxError $ getLineNumber h4
                            else SyntaxError $ getLineNumber h3
                    else SyntaxError $ getLineNumber h2
            else SyntaxError $ getLineNumber h1
    | otherwise = SyntaxError $ getLineNumber h
parseArrayType [] = SyntaxError (-1)

parseMaximumIndex :: [TokenInfo] -> Parse (MaximumIndex, [TokenInfo])
parseMaximumIndex tokens = do
    (integer, restTokens) <- parseInteger_ tokens
    return (MaximumIndex integer, restTokens)

parseMinimumIndex :: [TokenInfo] -> Parse (MinimumIndex, [TokenInfo])
parseMinimumIndex tokens = do
    (integer, restTokens) <- parseInteger_ tokens
    return (MinimumIndex integer, restTokens)

parseInteger_ :: [TokenInfo] -> Parse (Integer_, [TokenInfo])
parseInteger_ (h:t)
    | getTokenType h `elem` ["SPLUS", "SMINUS"] = do
        (sign, restTokens) <- parseSign (h:t)
        (unsignedInteger, restTokens1) <- parseUnsignedInteger restTokens
        return (Integer_ (Just sign) unsignedInteger, restTokens1)
    | otherwise = do
        (unsignedInteger, restTokens) <- parseUnsignedInteger (h:t)
        return (Integer_ Nothing unsignedInteger, restTokens)
parseInteger_ [] = SyntaxError (-1)

parseSign :: [TokenInfo] -> Parse (Sign, [TokenInfo])
parseSign (h:t)
    | getTokenType h `elem` ["SPLUS", "SMINUS"] = return (Sign h, t)
    | otherwise = SyntaxError $ getLineNumber h
parseSign [] = SyntaxError (-1)

parseSubprogramDeclarations :: [TokenInfo] -> Parse (SubprogramDeclarations, [TokenInfo])
parseSubprogramDeclarations tokens = do
    (subprogramDeclarationList, restTokens) <- parseSubprogramDeclarationList tokens
    return (SubprogramDeclarations subprogramDeclarationList, restTokens)
    where
        parseSubprogramDeclarationList :: [TokenInfo] -> Parse ([SubprogramDeclaration], [TokenInfo])
        parseSubprogramDeclarationList (h:t)
            | getTokenType h == "SPROCEDURE" = do
                (subprogramDeclaration, restTokens) <- parseSubprogramDeclaration (h:t)
                if null restTokens then SyntaxError $ getLineNumber h
                else
                    let h1:t1 = restTokens in
                    if getTokenType h1 == "SSEMICOLON" then do
                        (subprogramDeclarationList, restTokens1) <- parseSubprogramDeclarationList t1
                        return (subprogramDeclaration : subprogramDeclarationList, restTokens1)
                    else SyntaxError $ getLineNumber h1
            | otherwise = return ([], h:t)
        parseSubprogramDeclarationList [] = return ([], [])

parseSubprogramDeclaration :: [TokenInfo] -> Parse (SubprogramDeclaration, [TokenInfo])
parseSubprogramDeclaration tokens = do
    (subprogramHead, restTokens) <- parseSubprogramHead tokens
    (variableDeclaration, restTokens1) <- parseVariableDeclaration restTokens
    (compoundStatement, restTokens2) <- parseCompoundStatement restTokens1
    return (SubprogramDeclaration subprogramHead variableDeclaration compoundStatement, restTokens2)

parseSubprogramHead :: [TokenInfo] -> Parse (SubprogramHead, [TokenInfo])
parseSubprogramHead (h:t)
    | getTokenType h == "SPROCEDURE" = do
        (procedureName, restTokens) <- parseProcedureName t
        (parameter, restTokens1) <- parseParameter restTokens
        if null restTokens1 then SyntaxError $ getLineNumber h
        else
            let h1:t1 = restTokens1 in
            if getTokenType h1 == "SSEMICOLON" then return (SubprogramHead procedureName parameter, t1)
            else SyntaxError $ getLineNumber h1
    | otherwise = SyntaxError $ getLineNumber h
parseSubprogramHead [] = SyntaxError (-1)

parseProcedureName :: [TokenInfo] -> Parse (ProcedureName, [TokenInfo])
parseProcedureName tokens = do
    (identifier, restTokens) <- parseIdentifier tokens
    return (ProcedureName identifier, restTokens)

parseParameter :: [TokenInfo] -> Parse (Parameter, [TokenInfo])
parseParameter (h:t)
    | getTokenType h == "SLPAREN" = do
        (parameterSequence, restTokens) <- parseParameterSequence t
        if null restTokens then SyntaxError $ getLineNumber h
        else
            let h1:t1 = restTokens in
            if getTokenType h1 == "SRPAREN" then return (Parameter (Just parameterSequence), t1)
            else SyntaxError $ getLineNumber h1
    | otherwise = return (Parameter Nothing, h:t)
parseParameter [] = SyntaxError (-1)

parseParameterSequence :: [TokenInfo] -> Parse (ParameterSequence, [TokenInfo])
parseParameterSequence tokens = do
    (parameterSequence', restTokens) <- parseParameterSequence' tokens
    (parameterSequence'List, restTokens1) <- parseParameterSequence'List restTokens
    return (ParameterSequence parameterSequence' parameterSequence'List, restTokens1)
    where
        parseParameterSequence'List :: [TokenInfo] -> Parse ([ParameterSequence'], [TokenInfo])
        parseParameterSequence'List (h:t)
            | getTokenType h == "SSEMICOLON" = do
                (parameterSequence', restTokens) <- parseParameterSequence' t
                (parameterSequence'List, restTokens1) <- parseParameterSequence'List restTokens
                return (parameterSequence' : parameterSequence'List, restTokens1)
            | otherwise = return ([], h:t)
        parseParameterSequence'List [] = return ([], [])

parseParameterSequence' :: [TokenInfo] -> Parse (ParameterSequence', [TokenInfo])
parseParameterSequence' (h:t) = do
    (parameterNameSequence, restTokens) <- parseParameterNameSequence (h:t)
    if null restTokens then SyntaxError $ getLineNumber h
    else
        let h1:t1 = restTokens in
        if getTokenType h1 == "SCOLON" then do
            (standardType, restTokens1) <- parseStandardType t1
            return (ParameterSequence' parameterNameSequence standardType, restTokens1)
        else SyntaxError $ getLineNumber h1
parseParameterSequence' [] = SyntaxError (-1)

parseParameterNameSequence :: [TokenInfo] -> Parse (ParameterNameSequence, [TokenInfo])
parseParameterNameSequence (h:t) = do
    (parameterName, restTokens) <- parseParameterName (h:t)
    (parameterNameList, restTokens1) <- parseParameterNameList restTokens
    return (ParameterNameSequence parameterName parameterNameList, restTokens1)
    where
        parseParameterNameList :: [TokenInfo] -> Parse ([ParameterName], [TokenInfo])
        parseParameterNameList (h:t)
            | getTokenType h == "SCOMMA" = do
                (parameterName, restTokens) <- parseParameterName t
                (parameterNameList, restTokens1) <- parseParameterNameList restTokens
                return (parameterName : parameterNameList, restTokens1)
            | otherwise = return ([], h:t)
        parseParameterNameList [] = return ([], [])
parseParameterNameSequence [] = SyntaxError (-1)

parseParameterName :: [TokenInfo] -> Parse (ParameterName, [TokenInfo])
parseParameterName tokens = do
    (identifier, restTokens) <- parseIdentifier tokens
    return (ParameterName identifier, restTokens)

parseCompoundStatement :: [TokenInfo] -> Parse (CompoundStatement, [TokenInfo])
parseCompoundStatement (h:t)
    | getTokenType h == "SBEGIN" = do
        (statementSequence, restTokens) <- parseStatementSequence t
        if null restTokens then SyntaxError $ getLineNumber h
        else
            let h1:t1 = restTokens in
            if getTokenType h1 == "SEND" then return (CompoundStatement statementSequence, t1)
            else SyntaxError $ getLineNumber h1
    | otherwise = SyntaxError $ getLineNumber h
parseCompoundStatement [] = SyntaxError (-1)

parseStatementSequence :: [TokenInfo] -> Parse (StatementSequence, [TokenInfo])
parseStatementSequence (h:t) = do
    (statement, restTokens) <- parseStatement (h:t)
    if null restTokens then SyntaxError $ getLineNumber h
    else
        let h1:t1 = restTokens in
        if getTokenType h1 == "SSEMICOLON" then do
            (statementList, restTokens1) <- parseStatementList t1
            return (StatementSequence statement statementList, restTokens1)
        else SyntaxError $ getLineNumber h1
        where
            parseStatementList :: [TokenInfo] -> Parse ([Statement], [TokenInfo])
            parseStatementList (h:t)
                | getTokenType h `elem` ["SIDENTIFIER", "SREADLN", "SWRITELN", "SBEGIN", "SIF", "SWHILE"] = do
                    (statement, restTokens) <- parseStatement (h:t)
                    if null restTokens then SyntaxError $ getLineNumber h
                    else
                        let h1:t1 = restTokens in
                        if getTokenType h1 == "SSEMICOLON" then do
                            (statementList, restTokens1) <- parseStatementList t1
                            return (statement : statementList, restTokens1)
                        else SyntaxError $ getLineNumber h1
                | otherwise = return ([], h:t)
            parseStatementList [] = return ([], [])
parseStatementSequence [] = SyntaxError (-1)

parseStatement :: [TokenInfo] -> Parse (Statement, [TokenInfo])
parseStatement (h:t)
    | getTokenType h `elem` ["SIDENTIFIER", "SBEGIN", "SREADLN", "SWRITELN"] = do
        (basicStatement, restTokens) <- parseBasicStatement (h:t)
        return (Basic basicStatement, restTokens)
    | getTokenType h == "SIF" = do
        (ifStatement, restTokens) <- parseIfStatement (h:t)
        return (Branch ifStatement, restTokens)
    | getTokenType h == "SWHILE" = do
        (whileStatement, restTokens) <- parseWhileStatement (h:t)
        return (Repeat whileStatement, restTokens)
    | otherwise = SyntaxError $ getLineNumber h
parseStatement [] = SyntaxError (-1)

parseIfStatement :: [TokenInfo] -> Parse (IfStatement, [TokenInfo])
parseIfStatement (h:t)
    | getTokenType h == "SIF" = do
        (expression, restTokens) <- parseExpression t
        if null restTokens then SyntaxError $ getLineNumber h
        else
            let h1:t1 = restTokens in
            if getTokenType h1 == "STHEN" then do
                (compoundStatement, restTokens1) <- parseCompoundStatement t1
                (elseStatement, restTokens2) <- parseElseStatement restTokens1
                return (IfStatement expression compoundStatement elseStatement, restTokens2)
            else SyntaxError $ getLineNumber h1
    | otherwise = SyntaxError $ getLineNumber h
parseIfStatement [] = SyntaxError (-1)

parseElseStatement :: [TokenInfo] -> Parse (ElseStatement, [TokenInfo])
parseElseStatement (h:t)
    | getTokenType h == "SELSE" = do
        (compoundStatement, restTokens) <- parseCompoundStatement t
        return (ElseStatement (Just compoundStatement), restTokens)
    | otherwise = return (ElseStatement Nothing, h:t)
parseElseStatement [] = SyntaxError (-1)

parseWhileStatement :: [TokenInfo] -> Parse (WhileStatement, [TokenInfo])
parseWhileStatement (h:t)
    | getTokenType h == "SWHILE" = do
        (expression, restTokens) <- parseExpression t
        if null restTokens then SyntaxError $ getLineNumber h
        else
            let h1:t1 = restTokens in
            if getTokenType h1 == "SDO" then do
                (compoundStatement, restTokens1) <- parseCompoundStatement t1
                return (WhileStatement expression compoundStatement, restTokens1)
            else SyntaxError $ getLineNumber h1
    | otherwise = SyntaxError $ getLineNumber h
parseWhileStatement [] = SyntaxError (-1)

parseBasicStatement :: [TokenInfo] -> Parse (BasicStatemet, [TokenInfo])
parseBasicStatement (h:h1:t)
    | getTokenType h == "SIDENTIFIER"= do
        if getTokenType h1 `elem` ["SASSIGN", "SLBRACKET"] then do
            (assignmentStatement, restTokens) <- parseAssignmentStatement (h:h1:t)
            return (Assignment assignmentStatement, restTokens)
        else do
            (procedureCallStatement, restTokens) <- parseProcedureCallStatement (h:h1:t)
            return (ProcedureCall procedureCallStatement, restTokens)
    | getTokenType h `elem` ["SREADLN", "SWRITELN"] = do
        (ioStatement, restTokens) <- parseIOStatement (h:h1:t)
        return (IO ioStatement, restTokens)
    | getTokenType h == "SBEGIN" = do
        (compoundStatement, restTokens) <- parseCompoundStatement (h:h1:t)
        return (Compound compoundStatement, restTokens)
    | otherwise = SyntaxError $ getLineNumber h
parseBasicStatement (h:t)
    | getTokenType h `elem` ["SREADLN", "SWRITELN"] = do
        (ioStatement, restTokens) <- parseIOStatement (h:t)
        return (IO ioStatement, restTokens)
    | getTokenType h == "SBEGIN" = do
        (compoundStatement, restTokens) <- parseCompoundStatement (h:t)
        return (Compound compoundStatement, restTokens)
    | otherwise = SyntaxError $ getLineNumber h
parseBasicStatement [] = SyntaxError (-1)

parseAssignmentStatement :: [TokenInfo] -> Parse (AssignmentStatement, [TokenInfo])
parseAssignmentStatement (h:t) = do
    (leftSide, restTokens) <- parseLeftSide (h:t)
    if null restTokens then SyntaxError $ getLineNumber h
    else
        let h1:t1 = restTokens in
        if getTokenType h1 == "SASSIGN" then do
            (expression, restTokens1) <- parseExpression t1
            return (AssignmentStatement leftSide expression, restTokens1)
        else SyntaxError $ getLineNumber h1
parseAssignmentStatement [] = SyntaxError (-1)

parseLeftSide :: [TokenInfo] -> Parse (LeftSide, [TokenInfo])
parseLeftSide tokens = do
    (variable, restTokens) <- parseVariable tokens
    return (LeftSide variable, restTokens)

parseVariable :: [TokenInfo] -> Parse (Variable, [TokenInfo])
parseVariable (h:h1:t)
    | getTokenType h1 == "SLBRACKET" = do
        (indexedVariable, restTokens) <- parseIndexedVariable (h:h1:t)
        return (Indexed indexedVariable, restTokens)
    | otherwise = do
        (pureVariable, restTokens) <- parsePureVariable (h:h1:t)
        return (Pure pureVariable, restTokens)
parseVariable tokens = do
    (pureVariable, restTokens) <- parsePureVariable tokens
    return (Pure pureVariable, restTokens)

parsePureVariable :: [TokenInfo] -> Parse (PureVariable, [TokenInfo])
parsePureVariable tokens = do
    (variableName, restTokens) <- parseVariableName tokens
    return (PureVariable variableName, restTokens)

parseIndexedVariable :: [TokenInfo] -> Parse (IndexedVariable, [TokenInfo])
parseIndexedVariable (h:t) = do
    (variableName, restTokens) <- parseVariableName (h:t)
    if null restTokens then SyntaxError $ getLineNumber h
    else
        let h1:t1 = restTokens in
        if getTokenType h1 == "SLBRACKET" then
        do
            (index, restTokens1) <- parseIndex t1
            if null restTokens1 then SyntaxError $ getLineNumber h1
            else
                let h2:t2 = restTokens1 in
                if getTokenType h2 == "SRBRACKET" then return (IndexedVariable variableName index, t2)
                else SyntaxError $ getLineNumber h2
        else SyntaxError $ getLineNumber h1
parseIndexedVariable [] = SyntaxError (-1)

parseIndex :: [TokenInfo] -> Parse (Index, [TokenInfo])
parseIndex tokens = do
    (expression, restTokens) <- parseExpression tokens
    return (Index expression, restTokens)

parseProcedureCallStatement :: [TokenInfo] -> Parse (ProcedureCallStatement, [TokenInfo])
parseProcedureCallStatement (h:t) = do
    (procedureName, restTokens) <- parseProcedureName (h:t)
    if null restTokens then return (ProcedureCallStatement procedureName Nothing, restTokens)
    else
        let h1:t1 = restTokens in
        if getTokenType h1 == "SLPAREN" then do
            (expressionSequence, restTokens1) <- parseExpressionSequence t1
            if null restTokens1 then SyntaxError $ getLineNumber h1
            else
                let h2:t2 = restTokens1 in
                if getTokenType h2 == "SRPAREN" then return (ProcedureCallStatement procedureName (Just expressionSequence), t2)
                else SyntaxError $ getLineNumber h2
        else return (ProcedureCallStatement procedureName Nothing, restTokens)
parseProcedureCallStatement [] = SyntaxError (-1)

parseExpressionSequence :: [TokenInfo] -> Parse (ExpressionSequence, [TokenInfo])
parseExpressionSequence tokens = do
    (expression, restTokens) <- parseExpression tokens
    (expressionList, restTokens1) <- parseExpressionList restTokens
    return (ExpressionSequence expression expressionList, restTokens1)
    where
        parseExpressionList :: [TokenInfo] -> Parse ([Expression], [TokenInfo])
        parseExpressionList (h:t)
            | getTokenType h == "SCOMMA" = do
                (expression, restTokens) <- parseExpression t
                (expressionList, restTokens1) <- parseExpressionList restTokens
                return (expression : expressionList, restTokens1)
            | otherwise = return ([], h:t)
        parseExpressionList [] = return ([], [])

parseExpression :: [TokenInfo] -> Parse (Expression, [TokenInfo])
parseExpression (h:t) = do
    (simpleExpression, restTokens) <- parseSimpleExpression (h:t)
    if null restTokens then return (Expression simpleExpression Nothing, restTokens)
    else
        let h1:t1 = restTokens in
        if getTokenType h1 `elem` ["SEQUAL", "SNOTEQUAL", "SLESS", "SLESSEQUAL", "SGREAT", "SGREATEQUAL"] then do
            (relationalOperation, restTokens1) <- parseRelationalOperation restTokens
            return (Expression simpleExpression (Just relationalOperation), restTokens1)
        else return (Expression simpleExpression Nothing, restTokens)
parseExpression [] = SyntaxError (-1)

parseRelationalOperation :: [TokenInfo] -> Parse (RelationalOperation, [TokenInfo])
parseRelationalOperation (h:t)
    | getTokenType h `elem` ["SEQUAL", "SNOTEQUAL", "SLESS", "SLESSEQUAL", "SGREAT", "SGREATEQUAL"] = do
        (relationalOperator, restTokens) <- parseRelationalOperator (h:t)
        (simpleExpression, restTokens1) <- parseSimpleExpression restTokens
        return (RelationalOperation relationalOperator simpleExpression, restTokens1)
    | otherwise = SyntaxError $ getLineNumber h
parseRelationalOperation [] = SyntaxError (-1)

parseSimpleExpression :: [TokenInfo] -> Parse (SimpleExpression, [TokenInfo])
parseSimpleExpression (h:t)
    | getTokenType h `elem` ["SPLUS", "SMINUS"] = do
        (sign, restTokens) <- parseSign (h:t)
        (term, restTokens1) <- parseTerm restTokens
        (additionalOperationList, restTokens2) <- parseAdditionalOperationList restTokens1
        return (SimpleExpression (Just sign) term additionalOperationList, restTokens2)
    | otherwise = do
        (term, restTokens) <- parseTerm (h:t)
        (additionalOperationList, restTokens1) <- parseAdditionalOperationList restTokens
        return (SimpleExpression Nothing term additionalOperationList, restTokens1)
    where
        parseAdditionalOperationList :: [TokenInfo] -> Parse ([AdditionalOperation], [TokenInfo])
        parseAdditionalOperationList (h:t)
            | getTokenType h `elem` ["SPLUS", "SMINUS", "SOR"] = do
                (additionalOperation, restTokens) <- parseAdditionalOperation (h:t)
                (additionalOperationList, restTokens1) <- parseAdditionalOperationList restTokens
                return (additionalOperation : additionalOperationList, restTokens1)
            | otherwise = return ([], h:t)
        parseAdditionalOperationList [] = return ([], [])
parseSimpleExpression [] = SyntaxError (-1)

parseAdditionalOperation :: [TokenInfo] -> Parse (AdditionalOperation, [TokenInfo])
parseAdditionalOperation (h:t)
    | getTokenType h `elem` ["SPLUS", "SMINUS", "SOR"] = do
        (additionalOperator, restTokens) <- parseAdditionalOperator (h:t)
        (term, restTokens1) <- parseTerm restTokens
        return (AdditionalOperation additionalOperator term, restTokens1)
    | otherwise = SyntaxError $ getLineNumber h
parseAdditionalOperation [] = SyntaxError (-1)

parseTerm :: [TokenInfo] -> Parse (Term, [TokenInfo])
parseTerm tokens = do
    (factor, restTokens) <- parseFactor tokens
    (multiplicativeOperationList, restTokens1) <- parseMultiplicativeOperationList restTokens
    return (Term factor multiplicativeOperationList, restTokens1)
    where
        parseMultiplicativeOperationList :: [TokenInfo] -> Parse ([MultiplicativeOperation], [TokenInfo])
        parseMultiplicativeOperationList (h:t)
            | getTokenType h `elem` ["SSTAR", "SDIVD", "SMOD", "SAND"] = do
                (multiplicativeOperation, restTokens) <- parseMultiplicativeOperation (h:t)
                (multiplicativeOperationList, restTokens1) <- parseMultiplicativeOperationList restTokens
                return (multiplicativeOperation : multiplicativeOperationList, restTokens1)
            | otherwise = return ([], h:t)
        parseMultiplicativeOperationList [] = return ([], [])

parseMultiplicativeOperation :: [TokenInfo] -> Parse (MultiplicativeOperation, [TokenInfo])
parseMultiplicativeOperation (h:t)
    | getTokenType h `elem` ["SSTAR", "SDIVD", "SMOD", "SAND"] = do
        (multiplicativeOperator, restTokens) <- parseMultiplicativeOperator (h:t)
        (factor, restTokens1) <- parseFactor restTokens
        return (MultiplicativeOperation multiplicativeOperator factor, restTokens1)
    | otherwise = SyntaxError $ getLineNumber h
parseMultiplicativeOperation [] = SyntaxError (-1)

parseFactor :: [TokenInfo] -> Parse (Factor, [TokenInfo])
parseFactor (h:t)
    | getTokenType h == "SIDENTIFIER" = do
        (variable, restToken) <- parseVariable (h:t)
        return (VariableReference variable, restToken)
    | getTokenType h `elem` ["SCONSTANT", "SSTRING", "STRUE", "SFALSE"] = do
        (constant, restTokens) <- parseConstant (h:t)
        return (ConstantReference constant, restTokens)
    | getTokenType h == "SLPAREN" = do
        if null t then SyntaxError $ getLineNumber h
        else do
            (expression, restTokens) <- parseExpression t
            let h1:t1 = restTokens
            if getTokenType h1 == "SRPAREN" then return (Recursion expression, t1)
            else SyntaxError $ getLineNumber h1
    | getTokenType h == "SNOT" = do
        if null t then SyntaxError $ getLineNumber h
        else do
            (factor, restTokens) <- parseFactor t
            return (Negation factor, restTokens)
    | otherwise = SyntaxError $ getLineNumber h
parseFactor [] = SyntaxError (-1)

parseRelationalOperator :: [TokenInfo] -> Parse (RelationalOperator, [TokenInfo])
parseRelationalOperator (h:t)
    | getTokenType h `elem` ["SEQUAL", "SNOTEQUAL", "SLESS", "SLESSEQUAL", "SGREAT", "SGREATEQUAL"] = return (RelationalOperator h, t)
    | otherwise = SyntaxError $ getLineNumber h
parseRelationalOperator [] = SyntaxError (-1)

parseAdditionalOperator :: [TokenInfo] -> Parse (AdditionalOperator, [TokenInfo])
parseAdditionalOperator (h:t)
    | getTokenType h `elem` ["SPLUS", "SMINUS", "SOR"] = return (AdditionalOperator h, t)
    | otherwise = SyntaxError $ getLineNumber h
parseAdditionalOperator [] = SyntaxError (-1)

parseMultiplicativeOperator :: [TokenInfo] -> Parse (MultiplicativeOperator, [TokenInfo])
parseMultiplicativeOperator (h:t)
    | getTokenType h `elem` ["SSTAR", "SDIVD", "SMOD", "SAND"] = return (MultiplicativeOperator h, t)
    | otherwise = SyntaxError $ getLineNumber h
parseMultiplicativeOperator [] = SyntaxError (-1)

parseIOStatement :: [TokenInfo] -> Parse (IOStatement, [TokenInfo])
parseIOStatement (h:h1:t)
    | getTokenType h == "SREADLN" = do
        if getTokenType h1 == "SLPAREN" then do
            (variableSequence, restTokens) <- parseVariableSequence t
            if null restTokens then SyntaxError $ getLineNumber h1
            else
                let h2:t1 = restTokens in
                if getTokenType h2 == "SRPAREN" then return (InputStatement (Just variableSequence), t1)
                else SyntaxError $ getLineNumber h2
        else return (InputStatement Nothing, h1:t)
    | getTokenType h == "SWRITELN" = do
        if getTokenType h1 == "SLPAREN" then do
            (expressionSequence, restTokens) <- parseExpressionSequence t
            if null restTokens then SyntaxError $ getLineNumber h1
            else
                let h2:t1 = restTokens in
                if getTokenType h2 == "SRPAREN" then return (OutputStatement (Just expressionSequence), t1)
                else SyntaxError $ getLineNumber h2
        else return (InputStatement Nothing, h1:t)
    | otherwise = SyntaxError $ getLineNumber h
parseIOStatement (h:t)
    | getTokenType h == "SREADLN" = return (InputStatement Nothing, t)
    | getTokenType h == "SWRITELN" = return (OutputStatement Nothing, t)
    | otherwise = SyntaxError $ getLineNumber h
parseIOStatement [] = SyntaxError (-1)

parseVariableSequence :: [TokenInfo] -> Parse (VariableSequence, [TokenInfo])
parseVariableSequence tokens = do
    (variable, restTokens) <- parseVariable tokens
    (variableList, restTokens1) <- parseVariableList restTokens
    return (VariableSequence variable variableList, restTokens1)
    where
        parseVariableList :: [TokenInfo] -> Parse ([Variable], [TokenInfo])
        parseVariableList (h:t)
            | getTokenType h == "SCOMMA" = do
                (variable, restTokens) <- parseVariable t
                (variableList, restTokens1) <- parseVariableList restTokens
                return (variable : variableList, restTokens1)
            | otherwise = return ([], h:t)
        parseVariableList [] = return ([], [])

parseConstant :: [TokenInfo] -> Parse (Constant, [TokenInfo])
parseConstant (h:t)
    | getTokenType h == "SCONSTANT" = do
        (unsignedInteger, restTokens) <- parseUnsignedInteger (h:t)
        return (IntegerLiteral unsignedInteger, restTokens)
    | getTokenType h == "SSTRING" = do
        (string, restTokens) <- parseString_ (h:t)
        return (StringLiteral string, restTokens)
    | getTokenType h `elem` ["STRUE", "SFALSE"] = do
        (boolean, restTokens) <- parseBoolean (h:t)
        return (BooleanLiteral boolean, restTokens)
    | otherwise = SyntaxError $ getLineNumber h
parseConstant [] = SyntaxError (-1)

parseUnsignedInteger :: [TokenInfo] -> Parse (UnsignedInteger, [TokenInfo])
parseUnsignedInteger (h:t)
    | getTokenType h == "SCONSTANT" = return (UnsignedInteger h, t)
    | otherwise = SyntaxError $ getLineNumber h
parseUnsignedInteger [] = SyntaxError (-1)

parseString_ :: [TokenInfo] -> Parse (String_, [TokenInfo])
parseString_ (h:t)
    | getTokenType h == "SSTRING" = return (String_ h, t)
    | otherwise = SyntaxError $ getLineNumber h
parseString_ [] = SyntaxError (-1)

parseBoolean :: [TokenInfo] -> Parse (Boolean, [TokenInfo])
parseBoolean (h:t)
    | getTokenType h `elem` ["STRUE", "SFALSE"] = return (Boolean h, t)
    | otherwise = SyntaxError $ getLineNumber h
parseBoolean [] = SyntaxError (-1)

parseIdentifier :: [TokenInfo] -> Parse (Identifier, [TokenInfo])
parseIdentifier (h:t)
    | getTokenType h == "SIDENTIFIER" = return (Identifier h, t)
    | otherwise = SyntaxError $ getLineNumber h
parseIdentifier [] = SyntaxError (-1)