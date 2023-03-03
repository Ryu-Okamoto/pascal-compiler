{-# LANGUAGE InstanceSigs #-}

module Parser where

import Lexer
import AST


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


parseSign :: [TokenInfo] -> Parse (Sign, [TokenInfo])
parseSign (h:t)
    | getTokenType h `elem` ["SPLUS", "SMINUS"] = return (Sign h, t)
    | otherwise = SyntaxError $ getLineNumber h
parseSign [] = SyntaxError (-1)

parseVariable :: [TokenInfo] -> Parse (Variable, [TokenInfo])
parseVariable = undefined

parseExpression :: [TokenInfo] -> Parse (Expression, [TokenInfo])
parseExpression tokens = do
    (simpleExpression, restTokens) <- parseSimpleExpression tokens
    (optionalRelationalOperation, restTokens1) <- parseOptionalRelationalOperation restTokens
    return (Expression simpleExpression optionalRelationalOperation, restTokens1)
    where
        parseOptionalRelationalOperation :: [TokenInfo] -> Parse (Optional RelationalOperation, [TokenInfo])
        parseOptionalRelationalOperation (h:t)
            | getTokenType h `elem` ["SEQUAL", "SNOTEQUAL", "SLESS", "SLESSEQUAL", "SGREAT", "SGREATEQUAL"] = do
                (relationalOperation, restTokens) <- parseRelationalOperation (h:t)
                return (Just relationalOperation, restTokens)
            | otherwise = return (Nothing, h:t)
        parseOptionalRelationalOperation [] = return (Nothing, [])

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

{-
parseIOStatement :: [TokenInfo] -> Parse (IOStatement, [TokenInfo])
parseVariableSequence :: [TokenInfo] -> Parse (VariableSequence, [TokenInfo])
-}


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