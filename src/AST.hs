module Src.AST where

import Src.Token ( Token )

type    AST                           = AProgram
data    AProgram                      = AProgram AProgramName ABlock ACompoundStatement
newtype AProgramName                  = AProgramName AIdentifier
data    ABlock                        = ABlock AVariableDeclaration ASubprogramDeclarations
newtype AVariableDeclaration          = AVariableDeclaration (Maybe AVariableDeclarationSequence)
data    AVariableDeclarationSequence  = AVariableDeclarationSequence  AVariableDeclarationSequence' [AVariableDeclarationSequence']
data    AVariableDeclarationSequence' = AVariableDeclarationSequence' AVariableNameSequence AType
data    AVariableNameSequence         = AVariableNameSequence AVariableName [AVariableName]
newtype AVariableName                 = AVariableName AIdentifier
data    AType                         = AStandard AStandardType | AArray AArrayType
newtype AStandardType                 = AStandardType Token
data    AArrayType                    = AArrayType AMinimumIndex AMaximumIndex AStandardType
newtype AMinimumIndex                 = AMinimumIndex AInteger
newtype AMaximumIndex                 = AMaximumIndex AInteger
data    AInteger                      = AInteger (Maybe ASign) AUnsignedInteger
newtype ASign                         = ASign Token
newtype ASubprogramDeclarations       = ASubprogramDeclarations [ASubprogramDeclaration]
data    ASubprogramDeclaration        = ASubprogramDeclaration ASubprogramHead AVariableDeclaration ACompoundStatement
data    ASubprogramHead               = ASubprogramHead AProcedureName AParameter
newtype AProcedureName                = AProcedureName AIdentifier
newtype AParameter                    = AParameter (Maybe AParameterSequence)
data    AParameterSequence            = AParameterSequence AParameterSequence' [AParameterSequence']
data    AParameterSequence'           = AParameterSequence' AParameterNameSequence AStandardType
data    AParameterNameSequence        = AParameterNameSequence AParameterName [AParameterName]
newtype AParameterName                = AParameterName AIdentifier
newtype ACompoundStatement            = ACompoundStatement AStatementSequence
data    AStatementSequence            = AStatementSequence AStatement [AStatement]
data    AStatement                    = ABasic ABasicStatemet | ABranch AIfStatement | ARepeat AWhileStatement
data    AIfStatement                  = AIfStatement AIfKeyWord AExpression ACompoundStatement AElseStatement
newtype AIfKeyWord                    = AIfKeyword Token            -- 意味解析時のエラー行表示用
newtype AElseStatement                = AElseStatement (Maybe ACompoundStatement)
data    AWhileStatement               = AWhileStatement AWhileKeyword AExpression ACompoundStatement
newtype AWhileKeyword                 = AWhileKeyword Token         -- 意味解析時のエラー行表示用
data    ABasicStatemet                = AAssignment AAssignmentStatement | AProcedureCall AProcedureCallStatement | AIO AIOStatement | ACompound ACompoundStatement
data    AAssignmentStatement          = AAssignmentStatement ALeftSide AExpression
newtype ALeftSide                     = ALeftSide AVariable
data    AVariable                     = APure APureVariable | AIndexed AIndexedVariable
newtype APureVariable                 = APureVariable AVariableName
data    AIndexedVariable              = AIndexedVariable AVariableName AIndex
newtype AIndex                        = AIndex AExpression
data    AProcedureCallStatement       = AProcedureCallStatement AProcedureName (Maybe AExpressionSequence)
data    AExpressionSequence           = AExpressionSequence AExpression [AExpression]
data    AExpression                   = AExpression ASimpleExpression (Maybe ARelationalOperation)
data    ARelationalOperation          = ARelationalOperation ARelationalOperator ASimpleExpression
data    ASimpleExpression             = ASimpleExpression (Maybe ASign) ATerm [AAdditionalOperation]
data    AAdditionalOperation          = AAdditionalOperation AAdditionalOperator ATerm
data    ATerm                         = ATerm AFactor [AMultiplicativeOperation]
data    AMultiplicativeOperation      = AMultiplicativeOperation AMultiplicativeOperator AFactor
data    AFactor                       = AVariableReference AVariable | AConstantReference AConstant | ARecursion AExpression | ANegation ANegationOperator AFactor
newtype ANegationOperator             = ANegationOperator Token     -- 意味解析時のエラー行表示用
newtype ARelationalOperator           = ARelationalOperator Token
newtype AAdditionalOperator           = AAdditionalOperator Token
newtype AMultiplicativeOperator       = AMultiplicativeOperator Token
data    AIOStatement                  = AInputStatement (Maybe AVariableSequence) | AOutputStatement (Maybe AExpressionSequence)
data    AVariableSequence             = AVariableSequence AVariable [AVariable]
data    AConstant                     = AIntegerLiteral AUnsignedInteger | AStringLiteral AString | ABooleanLiteral ABoolean | ACharacterLiteral ACharacter -- 一文字の場合は代入化にするため
newtype AUnsignedInteger              = AUnsignedInteger Token
newtype AString                       = AString Token
newtype ABoolean                      = ABoolean Token
newtype ACharacter                    = ACharacter Token
newtype AIdentifier                   = AIdentifier Token
