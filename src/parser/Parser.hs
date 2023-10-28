module Src.Parser.Parser ( run, Parse (..) ) where

import Src.Synonym ( LineNumber )
import Src.Token ( Token (..) )
import Src.AST

data Parse a = Parse a | SyntaxError LineNumber
instance Functor Parse where
    fmap :: (a -> b) -> Parse a -> Parse b
    fmap f (Parse x) = Parse (f x)
    fmap _ (SyntaxError lineNumber) = SyntaxError lineNumber
instance Applicative Parse where
    pure :: a -> Parse a
    pure = Parse
    (<*>) :: Parse (a -> b) -> Parse a -> Parse b
    (<*>) (Parse f) (Parse x) = Parse (f x)
    (<*>) _ (SyntaxError lineNumber) = SyntaxError lineNumber
    (<*>) (SyntaxError lineNumber) _ = SyntaxError lineNumber
instance Monad Parse where
    (>>=) :: Parse a -> (a -> Parse b) -> Parse b
    (>>=) (Parse x) f = f x
    (>>=) (SyntaxError lineNumber) _ = SyntaxError lineNumber

{-
    実装方針：
     - Token を一つずつ読んでLL(1)解析
      - 各 AST 要素に対し parse
       - 文法と異なるとこがあればその行番号とともに SyntaxError
       - そうでない場合は (AST 部分木, 残りの Token リスト) を返す
    
    例）以下、EBNF の式に対する parse 関数

    (1)「<A> ::= "fuga1" <B1> "fuga2" <B2> "fuga3"」のとき
    parseA tokens = 
        if (head tokens /= "fuga1") then SyntaxError ("fuga1" の行番号)
        else do
            (b1, rest1) <- parseB1 (tail token)
            if (head rest /= "fuga2") then SyntaxError ("fuga2" の行番号)
            else do
                (b2, rest2) <- parseB2 (tail rest1)
                if (head rest2 /= "fuga3") then SyntaxError ("fuga3" の行番号)
                else
                    return (AConstructer B1 B2, rest3)
    
    (2)「<C> ::= <D> | <E>」 のとき
    parseC tokens
        | head tokens `elem` First(<D>) = parseD tokens
        | head tokens `elem` First(<E>) = parseE tokens
        | otherwise = SyntaxEror (head tokens の行番号)
        where
            First(<X>) は <X> の First 集合、すなわち、<X> の要素で初めに現れうる終端記号の集合を表す。
-}

run :: [Token] -> Parse AST
run ts = do
    (program, _) <- parseProgram ts
    return program

parseProgram :: [Token] -> Parse (AProgram, [Token])
parseProgram [] = SyntaxError ""
parseProgram (h:t)
    | getSType h == cSPROGRAM = do
        (programName, rest1) <- parseProgramName t
        if null rest1
        then
            SyntaxError $ getSLineNumber h
        else
            if getSType (head rest1) /= cSEMICOLON
            then
                SyntaxError $ getSLineNumber $ head rest1
            else do
                (block, rest2) <- parseBlock (tail rest1)
                (compoundStatement, rest3) <- parseCompoundStatement rest2
                if null rest3
                then
                    SyntaxError $ getSLineNumber $ head rest1
                else
                    if getSType (head rest3) /= cSDOT
                    then SyntaxError $ getSLineNumber $ head rest3
                    else return (AProgram programName block compoundStatement, tail rest3)
    | otherwise = SyntaxError $ getSLineNumber h

parseProgramName :: [Token] -> Parse (AProgramName, [Token])
parseProgramName ts = do
    (identifier, rest) <- parseIdentifier ts
    return (AProgramName identifier, rest)

parseBlock :: [Token] -> Parse (ABlock, [Token])
parseBlock ts = do
    (variableDeclaration, rest1) <- parseVariableDeclaration ts
    (subprogramDeclarations, rest2) <- parseSubprogramDeclarations rest1
    return (ABlock variableDeclaration subprogramDeclarations, rest2)

parseVariableDeclaration :: [Token] -> Parse (AVariableDeclaration, [Token])
parseVariableDeclaration [] = SyntaxError ""
parseVariableDeclaration (h:t)
    | getSType h == cSVAR = do
        (variableDeclarationSequence, rest) <- parseVariableDeclarationSequence t
        return (AVariableDeclaration (Just variableDeclarationSequence), rest)
    | otherwise = return (AVariableDeclaration Nothing, h:t)

parseVariableDeclarationSequence :: [Token] -> Parse (AVariableDeclarationSequence, [Token])
parseVariableDeclarationSequence ts = do
    (variableDeclarationSequence', rest1) <- parseVariableDeclarationSequence' ts
    (variableDeclarationSequence'List, rest2) <- parseVariableDeclarationSequence'List rest1
    return (AVariableDeclarationSequence variableDeclarationSequence' variableDeclarationSequence'List, rest2)
    where
        parseVariableDeclarationSequence'List :: [Token] -> Parse ([AVariableDeclarationSequence'], [Token])
        parseVariableDeclarationSequence'List [] = return ([], [])
        parseVariableDeclarationSequence'List (h:t)
            | getSType h == cSIDENTIFIER = do
                (variableDeclarationSequence', rest1) <- parseVariableDeclarationSequence' (h:t)
                (variableDeclarationSequence'List, rest2) <- parseVariableDeclarationSequence'List rest1
                return (variableDeclarationSequence' : variableDeclarationSequence'List, rest2)
            | otherwise = return ([], h:t)
 
parseVariableDeclarationSequence' :: [Token] -> Parse (AVariableDeclarationSequence', [Token])
parseVariableDeclarationSequence' [] = SyntaxError ""
parseVariableDeclarationSequence' (h:t)
    | getSType h == cSIDENTIFIER = do
        (variableNameSequence, rest1) <- parseVariableNameSequence (h:t)
        if null rest1
        then 
            SyntaxError $ getSLineNumber h
        else 
            if getSType (head rest1) /= cSCOLON
            then
                SyntaxError $ getSLineNumber $ head rest1
            else do
                (type_, rest2) <- parseType (tail rest1)
                if null rest2
                then
                    SyntaxError $ getSLineNumber $ head rest1
                else 
                    if getSType (head rest2) /= cSEMICOLON
                    then SyntaxError $ getSLineNumber $ head rest2
                    else return (AVariableDeclarationSequence' variableNameSequence type_, tail rest2)
    | otherwise = SyntaxError $ getSLineNumber h

parseVariableNameSequence :: [Token] -> Parse (AVariableNameSequence, [Token])
parseVariableNameSequence ts = do
    (variableName, rest1) <- parseVariableName ts
    (variableNameList, rest2) <- parseVariableNameList rest1
    return (AVariableNameSequence variableName variableNameList, rest2)
    where
        parseVariableNameList :: [Token] -> Parse ([AVariableName], [Token])
        parseVariableNameList [] = return ([], [])
        parseVariableNameList (h:t)
            | getSType h == cSCOMMA = do
                (variableName, rest1) <- parseVariableName t
                (variableNameList, rest2) <- parseVariableNameList rest1
                return (variableName : variableNameList, rest2)
            | otherwise = return ([], h:t)

parseVariableName :: [Token] -> Parse (AVariableName, [Token])
parseVariableName ts = do
    (identifier, rest) <- parseIdentifier ts
    return (AVariableName identifier, rest)

parseType :: [Token] -> Parse (AType, [Token])
parseType [] = SyntaxError ""
parseType (h:t)
    | getSType h `elem` [cSINTEGER, cSCHAR, cSBOOLEAN] = do
        (standardType, rest) <- parseStandardType (h:t)
        return (AStandard standardType, rest)
    | getSType h == cSARRAY = do
        (arrayType, rest) <- parseArrayType (h:t)
        return (AArray arrayType, rest)
    | otherwise = SyntaxError $ getSLineNumber h

parseStandardType :: [Token] -> Parse (AStandardType, [Token])
parseStandardType [] = SyntaxError ""
parseStandardType (h:t)
    | getSType h `elem` [cSINTEGER, cSCHAR, cSBOOLEAN] = return (AStandardType h, t)
    | otherwise = SyntaxError $ getSLineNumber h

parseArrayType :: [Token] -> Parse (AArrayType, [Token])
parseArrayType [] = SyntaxError ""
parseArrayType (h:t)
    | getSType h == cSARRAY =
        if null t || getSType (head t) /= cSLBRACKET
        then
            SyntaxError $ getSLineNumber h
        else do
            (minimumIndex, rest1) <- parseMinimumIndex (tail t)
            if null rest1
            then 
                SyntaxError $ getSLineNumber $ head t
            else
                if getSType (head rest1) /= cSRANGE
                then 
                    SyntaxError $ getSLineNumber $ head rest1
                else do
                    (maximumIndex, rest2) <- parseMaximumIndex (tail rest1)
                    if null rest2
                    then
                        SyntaxError $ getSLineNumber $ head rest1
                    else
                        if getSType (head rest2) /= cSRBRACKET || null (tail rest2)
                        then 
                            SyntaxError $ getSLineNumber $ head rest2
                        else
                            if getSType (rest2 !! 1) /= cSOF
                            then
                                SyntaxError $ getSLineNumber $ rest2 !! 1
                            else do
                                (standardType, rest3) <- parseStandardType (drop 2 rest2)
                                return (AArrayType minimumIndex maximumIndex standardType, rest3)
    | otherwise = SyntaxError $ getSLineNumber h


parseMinimumIndex :: [Token] -> Parse (AMinimumIndex, [Token])
parseMinimumIndex ts = do
    (integer, rest) <- parseInteger ts
    return (AMinimumIndex integer, rest)

parseMaximumIndex :: [Token] -> Parse (AMaximumIndex, [Token])
parseMaximumIndex ts = do
    (integer, rest) <- parseInteger ts
    return (AMaximumIndex integer, rest)

parseInteger :: [Token] -> Parse (AInteger, [Token])
parseInteger [] = SyntaxError ""
parseInteger (h:t)
    | getSType h `elem` [cSPLUS, cSMINUS] = do
        (sign, rest1) <- parseSign (h:t)
        (unsignedInteger, rest2) <- parseUnsignedInteger rest1
        return (AInteger (Just sign) unsignedInteger, rest2)
    | otherwise = do
        (unsignedInteger, rest) <- parseUnsignedInteger (h:t)
        return (AInteger Nothing unsignedInteger, rest)

parseSign :: [Token] -> Parse (ASign, [Token])
parseSign [] = SyntaxError ""
parseSign (h:t)
    | getSType h `elem` [cSPLUS, cSMINUS] = return (ASign h, t)
    | otherwise = SyntaxError $ getSLineNumber h

parseSubprogramDeclarations :: [Token] -> Parse (ASubprogramDeclarations, [Token])
parseSubprogramDeclarations ts = do
    (subprogramDeclarationList, rest) <- parseSubprogramDeclarationList ts
    return (ASubprogramDeclarations subprogramDeclarationList, rest)
    where
        parseSubprogramDeclarationList :: [Token] -> Parse ([ASubprogramDeclaration], [Token])
        parseSubprogramDeclarationList [] = return ([], [])
        parseSubprogramDeclarationList (h:t)
            | getSType h == cSPROCEDURE = do
                (subprogramDeclaration, rest1) <- parseSubprogramDeclaration (h:t)
                if null rest1
                then
                    SyntaxError $ getSLineNumber h
                else
                    if getSType (head rest1) /= cSEMICOLON
                    then
                        SyntaxError $ getSLineNumber $ head rest1
                    else do
                        (subprogramDeclarationList, rest2) <- parseSubprogramDeclarationList (tail rest1)
                        return (subprogramDeclaration : subprogramDeclarationList, rest2)
            | otherwise = return ([], h:t)

parseSubprogramDeclaration :: [Token] -> Parse (ASubprogramDeclaration, [Token])
parseSubprogramDeclaration ts = do
    (subprogramHead, rest1) <- parseSubprogramHead ts
    (variableDeclaration, rest2) <- parseVariableDeclaration rest1
    (compoundStatement, rest3) <- parseCompoundStatement rest2
    return (ASubprogramDeclaration subprogramHead variableDeclaration compoundStatement, rest3)

parseSubprogramHead :: [Token] -> Parse (ASubprogramHead, [Token])
parseSubprogramHead [] = SyntaxError ""
parseSubprogramHead (h:t)
    | getSType h == cSPROCEDURE = do
        (procedureName, rest1) <- parseProcedureName t
        (parameter, rest2) <- parseParameter rest1
        if null rest2
        then 
            SyntaxError $ getSLineNumber h
        else
            if getSType (head rest2) /= cSEMICOLON
            then SyntaxError $ getSLineNumber $ head rest2
            else return (ASubprogramHead procedureName parameter, tail rest2)
    | otherwise = SyntaxError $ getSLineNumber h

parseProcedureName :: [Token] -> Parse (AProcedureName, [Token])
parseProcedureName ts = do
    (identifier, rest) <- parseIdentifier ts
    return (AProcedureName identifier, rest)

parseParameter :: [Token] -> Parse (AParameter, [Token])
parseParameter [] = SyntaxError ""
parseParameter (h:t)
    | getSType h == cSLPAREN = do
        (parameterSequence, rest) <- parseParameterSequence t
        if null rest || getSType (head rest) /= cSRPAREN
        then SyntaxError $ getSLineNumber h
        else return (AParameter (Just parameterSequence), tail rest)
    | otherwise = return (AParameter Nothing, h:t)

parseParameterSequence :: [Token] -> Parse (AParameterSequence, [Token])
parseParameterSequence ts = do
    (parameterSequence', rest1) <- parseParameterSequence' ts
    (parameterSequence'List, rest2) <- parseParameterSequence'List rest1
    return (AParameterSequence parameterSequence' parameterSequence'List, rest2)
    where
        parseParameterSequence'List :: [Token] -> Parse ([AParameterSequence'], [Token])
        parseParameterSequence'List [] = return ([], [])
        parseParameterSequence'List (h:t)
            | getSType h == cSEMICOLON = do
                (parameterSequence', rest1) <- parseParameterSequence' t
                (parameterSequence'List, rest2) <- parseParameterSequence'List rest1
                return (parameterSequence' : parameterSequence'List, rest2)
            | otherwise = return ([], h:t)

parseParameterSequence' :: [Token] -> Parse (AParameterSequence', [Token])
parseParameterSequence' [] = SyntaxError ""
parseParameterSequence' (h:t) = do
    (parameterNameSequence, rest1) <- parseParameterNameSequence (h:t)
    if null rest1
    then 
        SyntaxError $ getSLineNumber h
    else 
        if getSType (head rest1) /= cSCOLON
        then
            SyntaxError $ getSLineNumber $ head rest1
        else do
            (standardType, rest2) <- parseStandardType (tail rest1)
            return (AParameterSequence' parameterNameSequence standardType, rest2)

parseParameterNameSequence :: [Token] -> Parse (AParameterNameSequence, [Token])
parseParameterNameSequence ts = do
    (parameterName, rest1) <- parseParameterName ts
    (parameterNameList, rest2) <- parseParameterNameList rest1
    return (AParameterNameSequence parameterName parameterNameList, rest2)
    where
        parseParameterNameList :: [Token] -> Parse ([AParameterName], [Token])
        parseParameterNameList [] = return ([], [])
        parseParameterNameList (h:t)
            | getSType h == cSCOMMA = do
                (parameterName, rest1) <- parseParameterName t
                (parameterNameList, rest2) <- parseParameterNameList rest1
                return (parameterName : parameterNameList, rest2)
            | otherwise = return ([], h:t) 

parseParameterName :: [Token] -> Parse (AParameterName, [Token])
parseParameterName ts = do
    (identifier, rest) <- parseIdentifier ts
    return (AParameterName identifier, rest)

parseCompoundStatement :: [Token] -> Parse (ACompoundStatement, [Token])
parseCompoundStatement [] = SyntaxError ""
parseCompoundStatement (h:t)
    | getSType h == cSBEGIN = do
        (statementSequence, rest) <- parseStatementSequence t
        if null rest || getSType (head rest) /= cSEND
        then SyntaxError $ getSLineNumber h
        else return (ACompoundStatement statementSequence, tail rest)
    | otherwise = SyntaxError $ getSLineNumber h

parseStatementSequence :: [Token] -> Parse (AStatementSequence, [Token])
parseStatementSequence (h:t) = do
    (statement, rest1) <- parseStatement (h:t)
    if null rest1
    then 
        SyntaxError $ getSLineNumber h
    else 
        if getSType (head rest1) /= cSEMICOLON
        then
            SyntaxError $ getSLineNumber (head rest1)
        else do 
            (statementList, rest2) <- parseStatementList (tail rest1)
            return (AStatementSequence statement statementList, rest2)
    where
        parseStatementList :: [Token] -> Parse ([AStatement], [Token])
        parseStatementList [] = return ([], [])
        parseStatementList (h:t)
            | getSType h `elem` [cSIDENTIFIER, cSREADLN, cSWRITELN, cSBEGIN, cSIF, cSWHILE] = do
                (statement, rest1) <- parseStatement (h:t)
                if null rest1
                then 
                    SyntaxError $ getSLineNumber h
                else 
                    if getSType (head rest1) /= cSEMICOLON
                    then
                        SyntaxError $ getSLineNumber (head rest1)
                    else do 
                        (statementList, rest2) <- parseStatementList (tail rest1)
                        return (statement : statementList, rest2)
            | otherwise = return ([], h:t)

parseStatement :: [Token] -> Parse (AStatement, [Token])
parseStatement [] = SyntaxError ""
parseStatement (h:t)
    | getSType h `elem` [cSIDENTIFIER, cSBEGIN, cSREADLN, cSWRITELN] = do
        (basicStatement, rest) <- parseBasicStatemet (h:t)
        return (ABasic basicStatement, rest)
    | getSType h == cSIF = do
        (ifStatement, rest) <- parseIfStatement (h:t)
        return (ABranch ifStatement, rest)
    | getSType h == cSWHILE = do
        (whileStatement, rest) <- parseWhileStatement (h:t)
        return (ARepeat whileStatement, rest)
    | otherwise = SyntaxError $ getSLineNumber h

parseIfStatement :: [Token] -> Parse (AIfStatement, [Token])
parseIfStatement [] = SyntaxError ""
parseIfStatement (h:t)
    | getSType h == cSIF = do
        (expression, rest1) <- parseExpression t
        if null rest1 || getSType (head rest1) /= cSTHEN
        then 
            SyntaxError $ getSLineNumber h
        else do
            (compoundStatement, rest2) <- parseCompoundStatement (tail rest1)
            (elseStatement, rest3) <- parseElseStatement rest2
            return (AIfStatement expression compoundStatement elseStatement, rest3)

parseElseStatement :: [Token] -> Parse (AElseStatement, [Token])
parseElseStatement [] = SyntaxError ""
parseElseStatement (h:t)
    | getSType h == cSELSE = do
        (compoundStatement, rest) <- parseCompoundStatement t
        return (AElseStatement (Just compoundStatement), rest)
    | otherwise = return (AElseStatement Nothing, h:t)

parseWhileStatement :: [Token] -> Parse (AWhileStatement, [Token])
parseWhileStatement [] = SyntaxError ""
parseWhileStatement (h:t)
    | getSType h == cSWHILE = do
        (expression, rest1) <- parseExpression t
        if null rest1 || getSType (head rest1) /= cSDO
        then 
            SyntaxError $ getSLineNumber h
        else do
            (compoundStatement, rest2) <- parseCompoundStatement (tail rest1)
            return (AWhileStatement expression compoundStatement, rest2)
    | otherwise = SyntaxError $ getSLineNumber h

parseBasicStatemet :: [Token] -> Parse (ABasicStatemet, [Token])
parseBasicStatemet [] = SyntaxError ""
parseBasicStatemet (h:t)
    | getSType h == cSIDENTIFIER = 
        if null t || getSType (head t) `notElem` [cSASSIGN, cSLBRACKET]
        then do
            (procedureCallStatement, rest) <- parseProcedureCallStatement (h:t)
            return (AProcedureCall procedureCallStatement, rest)
        else do
            (assignmentStatement, rest) <- parseAssignmentStatement (h:t)
            return (AAssignment assignmentStatement, rest)
    | getSType h `elem` [cSREADLN, cSWRITELN] = do
        (ioStatement, rest) <- parseIOStatement (h:t)
        return (AIO ioStatement, rest)
    | getSType h == cSBEGIN = do
        (compoundStatement, rest) <- parseCompoundStatement (h:t)
        return (ACompound compoundStatement, rest)
    | otherwise = SyntaxError $ getSLineNumber h

parseAssignmentStatement :: [Token] -> Parse (AAssignmentStatement, [Token])
parseAssignmentStatement [] = SyntaxError ""
parseAssignmentStatement (h:t) = do
    (leftSide, rest1) <- parseLeftSide (h:t)
    if null rest1 || getSType (head rest1) /= cSASSIGN
    then
        SyntaxError $ getSLineNumber h
    else do
        (expression, rest2) <- parseExpression (tail rest1)
        return (AAssignmentStatement leftSide expression, rest2)

parseLeftSide :: [Token] -> Parse (ALeftSide, [Token])
parseLeftSide ts = do
    (variable, rest) <- parseVariable ts
    return (ALeftSide variable, rest)

parseVariable :: [Token] -> Parse (AVariable, [Token])
parseVariable [] = SyntaxError ""
parseVariable (h:t) = 
    if null t || getSType (head t) /= cSLBRACKET
    then do
        (pureVariable, rest) <- parsePureVariable (h:t)
        return (APure pureVariable, rest)
    else do
        (indexedVariable, rest) <- parseIndexedVariable (h:t)
        return (AIndexed indexedVariable, rest)

parsePureVariable :: [Token] -> Parse (APureVariable, [Token])
parsePureVariable ts = do
    (variableName, rest) <- parseVariableName ts
    return (APureVariable variableName, rest)

parseIndexedVariable :: [Token] -> Parse (AIndexedVariable, [Token])
parseIndexedVariable [] = SyntaxError ""
parseIndexedVariable (h:t) = do
    (variableName, rest1) <- parseVariableName (h:t)
    if null rest1 || getSType (head rest1) /= cSLBRACKET
    then
        SyntaxError $ getSLineNumber h
    else do
        (index, rest2) <- parseIndex (tail rest1)
        if null rest2 || getSType (head rest2) /= cSRBRACKET
        then SyntaxError $ getSLineNumber $ head rest1
        else return (AIndexedVariable variableName index, tail rest2)

parseIndex :: [Token] -> Parse (AIndex, [Token])
parseIndex ts = do
    (expression, rest) <- parseExpression ts
    return (AIndex expression, rest)

parseProcedureCallStatement :: [Token] -> Parse (AProcedureCallStatement, [Token])
parseProcedureCallStatement ts = do
    (procedureName, rest1) <- parseProcedureName ts
    if null rest1 || getSType (head rest1) /= cSLPAREN
    then 
        return (AProcedureCallStatement procedureName Nothing, rest1)
    else do
        (expressionSequence, rest2) <- parseExpressionSequence (tail rest1)
        if null rest2 || getSType (head rest2) /= cSRPAREN
        then SyntaxError $ getSLineNumber $ head rest1
        else return (AProcedureCallStatement procedureName (Just expressionSequence), tail rest2)

parseExpressionSequence :: [Token] -> Parse (AExpressionSequence, [Token])
parseExpressionSequence ts = do
    (expression, rest1) <- parseExpression ts
    (expressionList, rest2) <- parseExpressionList rest1
    return (AExpressionSequence expression expressionList, rest2)
    where
        parseExpressionList :: [Token] -> Parse ([AExpression], [Token])
        parseExpressionList [] = return ([], [])
        parseExpressionList (h:t)
            | getSType h == cSCOMMA = do
                (expression, rest1) <- parseExpression t
                (expressionList, rest2) <- parseExpressionList rest1
                return (expression : expressionList, rest2)
            | otherwise = return ([], h:t)

parseExpression :: [Token] -> Parse (AExpression, [Token])
parseExpression ts = do
    (simpleExpression, rest1) <- parseSimpleExpression ts
    if null rest1 || getSType (head rest1) `notElem` [cSEQUAL, cSNOTEQUAL, cSLESS, cSLESSEQUAL, cSGREAT, cSGREATEQUAL] 
    then 
        return (AExpression simpleExpression Nothing, rest1)
    else do
        (relationalOperation, rest2) <- parseRelationalOperation rest1
        return (AExpression simpleExpression (Just relationalOperation), rest2)

parseRelationalOperation :: [Token] -> Parse (ARelationalOperation, [Token])
parseRelationalOperation [] = SyntaxError ""
parseRelationalOperation (h:t)
    | getSType h `elem` [cSEQUAL, cSNOTEQUAL, cSLESS, cSLESSEQUAL, cSGREAT, cSGREATEQUAL] = do
        (operator, rest1) <- parseRelationalOperator (h:t)
        (simpleExpression, rest2) <- parseSimpleExpression rest1
        return (ARelationalOperation operator simpleExpression, rest2)
    | otherwise = SyntaxError $ getSLineNumber h

parseSimpleExpression :: [Token] -> Parse (ASimpleExpression, [Token])
parseSimpleExpression [] = SyntaxError ""
parseSimpleExpression (h:t)
    | getSType h `elem` [cSPLUS, cSMINUS] = do
        (sign, rest1) <- parseSign (h:t)
        (term, rest2) <- parseTerm rest1
        (additionalOperationList, rest3) <- parseAdditionalOperationList rest2
        return (ASimpleExpression (Just sign) term additionalOperationList, rest3)
    | otherwise = do
        (term, rest1) <- parseTerm (h:t)
        (additionalOperationList, rest2) <- parseAdditionalOperationList rest1
        return (ASimpleExpression Nothing term additionalOperationList, rest2)
    where
        parseAdditionalOperationList :: [Token] -> Parse ([AAdditionalOperation], [Token])
        parseAdditionalOperationList [] = return ([], [])
        parseAdditionalOperationList (h:t)
            | getSType h `elem` [cSPLUS, cSMINUS, cSOR] = do
                (additionalOperation, rest1) <- parseAdditionalOperation (h:t)
                (additionalOperationList, rest2) <- parseAdditionalOperationList rest1
                return (additionalOperation : additionalOperationList, rest2)
            | otherwise = return ([], h:t)

parseAdditionalOperation :: [Token] -> Parse (AAdditionalOperation, [Token])
parseAdditionalOperation [] = SyntaxError ""
parseAdditionalOperation (h:t)
    | getSType h `elem` [cSPLUS, cSMINUS, cSOR] = do
        (operator, rest1) <- parseAdditionalOperator (h:t)
        (term, rest2) <- parseTerm rest1
        return (AAdditionalOperation operator term, rest2)
    | otherwise = SyntaxError $ getSLineNumber h

parseTerm :: [Token] -> Parse (ATerm, [Token])
parseTerm ts = do
    (factor, rest1) <- parseFactor ts
    (multiplicativeOperationList, rest2) <- parseMultiplicativeOperationList rest1
    return (ATerm factor multiplicativeOperationList, rest2)
    where
        parseMultiplicativeOperationList :: [Token] -> Parse ([AMultiplicativeOperation], [Token])
        parseMultiplicativeOperationList [] = return ([], [])
        parseMultiplicativeOperationList (h:t)
            | getSType h `elem` [cSSTAR, cSDIVD, cSMOD, cSAND] = do
                (multiplicativeOperation, rest1) <- parseMultiplicativeOperation (h:t)
                (multiplicativeOperationList, rest2) <- parseMultiplicativeOperationList rest1
                return (multiplicativeOperation : multiplicativeOperationList, rest2)
            | otherwise = return ([], h:t)

parseMultiplicativeOperation :: [Token] -> Parse (AMultiplicativeOperation, [Token])
parseMultiplicativeOperation [] = SyntaxError ""
parseMultiplicativeOperation (h:t)
    | getSType h `elem` [cSSTAR, cSDIVD, cSMOD, cSAND] = do
        (operator, rest1) <- parseMultiplicativeOperator (h:t)
        (factor, rest2) <- parseFactor rest1
        return (AMultiplicativeOperation operator factor, rest2)
    | otherwise = SyntaxError ""

parseFactor :: [Token] -> Parse (AFactor, [Token])
parseFactor [] = SyntaxError ""
parseFactor (h:t)
    | getSType h == cSIDENTIFIER = do
        (variable, rest) <- parseVariable (h:t)
        return (AVariableReference variable, rest)
    | getSType h `elem` [cSCONSTANT, cSSTRING, cSTRUE, cSFALSE] = do
        (constant, rest) <- parseConstant (h:t)
        return (AConstantReference constant, rest)
    | getSType h == cSLPAREN = do
        (expression, rest) <- parseExpression t
        if null rest || getSType (head rest) /= cSRPAREN 
        then SyntaxError $ getSLineNumber h
        else return (ARecursion expression, tail rest)
    | getSType h == cSNOT = do
        (factor, rest) <- parseFactor t
        return (ANegation factor, rest)
    | otherwise = SyntaxError $ getSLineNumber h

parseRelationalOperator :: [Token] -> Parse (ARelationalOperator, [Token])
parseRelationalOperator [] = SyntaxError ""
parseRelationalOperator (h:t)
    | getSType h `elem` [cSEQUAL, cSNOTEQUAL, cSLESS, cSLESSEQUAL, cSGREAT, cSGREATEQUAL] = return (ARelationalOperator h, t)
    |otherwise = SyntaxError $ getSLineNumber h

parseAdditionalOperator :: [Token] -> Parse (AAdditionalOperator, [Token])
parseAdditionalOperator [] = SyntaxError ""
parseAdditionalOperator (h:t)
    | getSType h `elem` [cSPLUS, cSMINUS, cSOR] = return (AAdditionalOperator h, t)
    | otherwise = SyntaxError $ getSLineNumber h

parseMultiplicativeOperator :: [Token] -> Parse (AMultiplicativeOperator, [Token])
parseMultiplicativeOperator [] = SyntaxError ""
parseMultiplicativeOperator (h:t)
    | getSType h `elem` [cSSTAR, cSDIVD, cSMOD, cSAND] = return (AMultiplicativeOperator h, t)
    | otherwise = SyntaxError $ getSLineNumber h

parseIOStatement :: [Token] -> Parse (AIOStatement, [Token])
parseIOStatement [] = SyntaxError ""
parseIOStatement (h1:h2:t)
    | getSType h1 == cSREADLN =
        if getSType h2 /= cSLPAREN 
        then 
            return (AInputStatement Nothing, h2:t)
        else do
            (variableSequence, rest) <- parseVariableSequence t
            if null rest || getSType (head rest) /= cSRPAREN 
            then SyntaxError $ getSLineNumber h2
            else return (AInputStatement (Just variableSequence), tail rest)
    | getSType h1 == cSWRITELN =
        if getSType h2 /= cSLPAREN 
        then 
            return (AOutputStatement Nothing, h2:t)
        else do
            (expressionSequence, rest) <- parseExpressionSequence t
            if null rest || getSType (head rest) /= cSRPAREN 
            then SyntaxError $ getSLineNumber h2
            else return (AOutputStatement (Just expressionSequence), tail rest)
    | otherwise = SyntaxError $ getSLineNumber h1
parseIOStatement (h:t)
    | getSType h == cSREADLN = return (AInputStatement Nothing, t)
    | getSType h == cSWRITELN = return (AOutputStatement Nothing, t)
    | otherwise = SyntaxError $ getSLineNumber h

parseVariableSequence :: [Token] -> Parse (AVariableSequence, [Token])
parseVariableSequence ts = do
    (variable, rest1) <- parseVariable ts
    (variableList, rest2) <- parseVariableList rest1
    return (AVariableSequence variable variableList, rest2)
    where
        parseVariableList :: [Token] -> Parse ([AVariable], [Token])
        parseVariableList [] = return ([], [])
        parseVariableList (h:t)
            | getSType h == cSCOMMA = do
                (variable, rest1) <- parseVariable t 
                (variableList, rest2) <- parseVariableList rest1
                return (variable : variableList, rest2)
            | otherwise = return ([], h:t)

parseConstant :: [Token] -> Parse (AConstant, [Token])
parseConstant [] = SyntaxError ""
parseConstant (h:t)
    | getSType h == cSCONSTANT = do
        (unsignedInteger, rest) <- parseUnsignedInteger (h:t)
        return (AIntegerLiteral unsignedInteger, rest)
    | getSType h == cSSTRING = do
        (string, rest) <- parseString (h:t)
        return (AStringLiteral string, rest)
    | getSType h `elem` [cSTRUE, cSFALSE] = do
        (boolean, rest) <- parseBoolean (h:t)
        return (ABooleanLiteral boolean, rest)

parseUnsignedInteger :: [Token] -> Parse (AUnsignedInteger, [Token])
parseUnsignedInteger [] = SyntaxError ""
parseUnsignedInteger (h:t)
    | getSType h == cSCONSTANT = return (AUnsignedInteger h, t)
    | otherwise = SyntaxError $ getSLineNumber h

parseString :: [Token] -> Parse (AString, [Token])
parseString [] = SyntaxError ""
parseString (h:t)
    | getSType h == cSSTRING = return (AString h, t)
    | otherwise = SyntaxError $ getSLineNumber h

parseBoolean :: [Token] -> Parse (ABoolean, [Token])
parseBoolean [] = SyntaxError ""
parseBoolean (h:t)
    | getSType h `elem` [cSTRUE, cSFALSE] = return (ABoolean h, t)
    | otherwise = SyntaxError $ getSLineNumber h

parseIdentifier :: [Token] -> Parse (AIdentifier, [Token])
parseIdentifier [] = SyntaxError ""
parseIdentifier (h:t)
    | getSType h == cSIDENTIFIER = return (AIdentifier h, t)
    | otherwise = SyntaxError $ getSLineNumber h


cSPROGRAM = "SPROGRAM" :: String
cSEMICOLON = "SSEMICOLON" :: String
cSDOT = "SDOT" :: String
cSVAR = "SVAR" :: String
cSIDENTIFIER = "SIDENTIFIER" :: String
cSCOLON = "SCOLON" :: String
cSCOMMA = "SCOMMA" :: String
cSINTEGER = "SINTEGER" :: String
cSCHAR = "SCHAR" :: String
cSBOOLEAN = "SBOOLEAN" :: String
cSARRAY = "SARRAY" :: String
cSLBRACKET = "SLBRACKET" :: String
cSRANGE = "SRANGE" :: String
cSRBRACKET = "SRBRACKET" :: String
cSOF = "SOF" :: String
cSPLUS = "SPLUS" :: String
cSMINUS = "SMINUS" :: String
cSPROCEDURE = "SPROCEDURE" :: String
cSLPAREN = "SLPAREN" :: String
cSRPAREN = "SRPAREN" :: String
cSBEGIN = "SBEGIN" :: String
cSEND = "SEND" :: String
cSREADLN = "SREADLN" :: String
cSWRITELN = "SWRITELN" :: String
cSIF = "SIF" :: String
cSWHILE = "SWHILE" :: String
cSTHEN = "STHEN" :: String
cSELSE = "SELSE" :: String
cSDO = "SDO" :: String
cSASSIGN = "SASSIGN" :: String
cSEQUAL = "SEQUAL" :: String
cSNOTEQUAL = "SNOTEQUAL" :: String
cSLESS = "SLESS" :: String
cSLESSEQUAL = "SLESSEQUAL" :: String
cSGREAT = "SGREAT" :: String
cSGREATEQUAL = "SGREATEQUAL" :: String
cSOR = "SOR" :: String
cSSTAR = "SSTAR" :: String
cSDIVD = "SDIVD" :: String
cSMOD = "SMOD" :: String
cSAND = "SAND" :: String
cSCONSTANT = "SCONSTANT" :: String
cSSTRING = "SSTRING" :: String
cSTRUE = "STRUE" :: String
cSFALSE = "SFALSE" :: String
cSNOT = "SNOT" :: String