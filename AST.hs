module AST where

import Lexer

type Optional = Maybe
type AST = Program

data    Program                      = Program ProgramName Block CompoundStatement
newtype ProgramName                  = ProgramName Identifier
data    Block                        = Block VariableDeclaration SubprogramDeclarations
newtype VariableDeclaration          = VariableDeclaration [VariableDeclarationSequence]
data    VariableDeclarationSequence  = VariableDeclarationSequence VariableDeclarationSequence' [VariableDeclarationSequence']
data    VariableDeclarationSequence' = VariableDeclarationSequence' VariableNameSequence Type
data    VariableNameSequence         = VariableNameSequence VariableName [VariableName]
newtype VariableName                 = VariableName Identifier
data    Type                         = Standard StandardType | Array ArrayType
newtype StandardType                 = StandardType TokenInfo
data    ArrayType                    = ArrayType MinimumIndex MaximumIndex StandardType
newtype MinimumIndex                 = MinimumIndex Integer_
newtype MaximumIndex                 = MaximumIndex Integer_
data    Integer_                     = Integer_ (Optional Sign) UnsignedInteger
newtype Sign                         = Sign TokenInfo
newtype SubprogramDeclarations       = SubprogramDeclarations [SubprogramDeclaration]
data    SubprogramDeclaration        = SubprogramDeclaration SubprogramHead VariableDeclaration CompoundStatement
data    SubprogramHead               = SubprogramHead ProcedureName Parameter
newtype ProcedureName                = ProcedureName Identifier
newtype Parameter                    = Parameter [ParameterSequence]
data    ParameterSequence            = ParameterSequence ParameterSequence' [ParameterSequence']
data    ParameterSequence'           = ParameterSequence' ParameterNameSequence StandardType
data    ParameterNameSequence        = ParameterNameSequence ParameterName [ParameterName]
newtype ParameterName                = ParameterName Identifier
newtype CompoundStatement            = CompoundStatement StatementSequence
data    StatementSequence            = StatementSequence Statement [Statement]
data    Statement                    = Basic BasicStatemet | Branch IfStatement | Repeat WhileStatement
data    IfStatement                  = IfStatement Expression CompoundStatement ElseStatement
newtype ElseStatement                = ElseStatement (Optional CompoundStatement)
data    WhileStatement               = WhileStatement Expression CompoundStatement
data    BasicStatemet                = Assign AssignStatement | ProcedureCall ProcedureCallStatement | IO IOStatement | Compound CompoundStatement
data    AssignStatement              = AssignStatement LeftSide Expression
newtype LeftSide                     = LeftSide Variable
data    Variable                     = Pure PureVariable | Indexed IndexedVariable
newtype PureVariable                 = PureVariable VariableName
data    IndexedVariable              = IndexedVariable VariableName Index
newtype Index                        = Index Expression
data    ProcedureCallStatement       = ProcedureCallStatement ProcedureName (Optional ExpressionSequence)
data    ExpressionSequence           = ExpressionSequence Expression [Expression]
data    Expression                   = Expression SimpleExpression (Optional RelationalOperation)
data    RelationalOperation          = RelationalOperation RelationalOperator SimpleExpression
data    SimpleExpression             = SimpleExpression (Optional Sign) Term [AdditionalOperation]
data    AdditionalOperation          = AdditionalOperation AdditionalOperator Term
data    Term                         = Term Factor [MultiplicativeOperation]
data    MultiplicativeOperation      = MultiplicativeOperation MultiplicativeOperator Factor
data    Factor                       = VariableReference Variable | ConstantReference Constant | Recursion Expression | Negation Factor
newtype RelationalOperator           = RelationalOperator TokenInfo
newtype AdditionalOperator           = AdditionalOperator TokenInfo
newtype MultiplicativeOperator       = MultiplicativeOperator TokenInfo
data    IOStatement                  = InputStatement (Optional VariableSequence) | OutputStatement (Optional ExpressionSequence)
data    VariableSequence             = VariableSequence Variable [Variable]
data    Constant                     = IntegerLiteral UnsignedInteger | StringLiteral String_ | BooleanLiteral Boolean
newtype UnsignedInteger              = UnsignedInteger TokenInfo
newtype String_                      = String_ TokenInfo
newtype Boolean                      = Boolean TokenInfo
newtype Identifier                   = Identifier TokenInfo

printAST :: AST -> IO ()
printAST = print . showAST

showAST :: AST -> String
showAST = showProgram 0 

makeIndent :: Int -> String
makeIndent 0 = ""
makeIndent n = makeIndent' n
    where makeIndent' :: Int ->  String
          makeIndent' 1 = " |- "
          makeIndent' n = " |  " ++ makeIndent' (n - 1)

showProgram :: Int -> Program -> String
showProgram n (Program programName block compoundStatement) = makeIndent n ++ "Program\n" ++ showProgramName (n + 1) programName ++ showBlock (n + 1) block ++ showCompoundStatement (n + 1) compoundStatement 
showProgramName :: Int -> ProgramName -> String
showProgramName n (ProgramName identifier) = makeIndent n ++ "ProgramName\n" ++ showIdentifier (n + 1) identifier 
showBlock :: Int -> Block -> String
showBlock n (Block variableDeclaration subprogramDeclarations) = makeIndent n ++ "Block\n" ++ showVariableDeclaration (n + 1) variableDeclaration ++ showSubprogramDeclarations (n + 1) subprogramDeclarations 
showVariableDeclaration :: Int -> VariableDeclaration -> String
showVariableDeclaration n (VariableDeclaration variableDeclarationSequenceList) = makeIndent n ++ "VariableDeclaration\n" ++ concatMap (showVariableDeclarationSequence (n + 1)) variableDeclarationSequenceList 
showVariableDeclarationSequence :: Int -> VariableDeclarationSequence -> String
showVariableDeclarationSequence n (VariableDeclarationSequence variableDeclarationSequence' variableDeclarationSequence'List) = makeIndent n ++ "VariableDeclarationSequence\n" ++ showVariableDeclarationSequence' (n + 1) variableDeclarationSequence' ++ concatMap (showVariableDeclarationSequence' (n + 1)) variableDeclarationSequence'List
showVariableDeclarationSequence' :: Int -> VariableDeclarationSequence' -> String
showVariableDeclarationSequence' n (VariableDeclarationSequence' variableNameSequence type_) = makeIndent n ++ "VariableDeclarationSequence'\n" ++ showVariableNameSequence (n + 1) variableNameSequence ++ showType (n + 1) type_ 
showVariableNameSequence :: Int -> VariableNameSequence -> String
showVariableNameSequence n (VariableNameSequence variableName variableNameList) = makeIndent n ++ "VariableNameSequence\n" ++ showVariableName (n + 1) variableName ++ concatMap (showVariableName (n + 1)) variableNameList 
showVariableName :: Int -> VariableName -> String
showVariableName n (VariableName identifier) = makeIndent n ++ "VariableName\n" ++ showIdentifier (n + 1) identifier 
showType :: Int -> Type -> String
showType n (Standard standardType) = makeIndent n ++ "Type\n" ++ showStandardType (n + 1) standardType
showType n (Array arrayType) = makeIndent n ++ "Type\n" ++ showArrayType (n + 1) arrayType
showStandardType :: Int -> StandardType -> String
showStandardType n (StandardType tokenInfo) = makeIndent n ++ "StandardType -> \"" ++ getToken tokenInfo ++ "\"\n"
showArrayType :: Int -> ArrayType -> String
showArrayType n (ArrayType minimumIndex maximumIndex standardType) = makeIndent n ++ "ArrayType\n" ++ showMinimumIndex (n + 1) minimumIndex ++ showMaximumIndex (n + 1) maximumIndex ++ showStandardType (n + 1) standardType 
showMinimumIndex :: Int -> MinimumIndex -> String
showMinimumIndex n (MinimumIndex integer) = makeIndent n ++ "MinimumIndex\n" ++ showInteger_ (n + 1) integer 
showMaximumIndex :: Int -> MaximumIndex -> String
showMaximumIndex n (MaximumIndex integer) = makeIndent n ++ "MaximumIndex\n" ++ showInteger_ (n + 1) integer 
showInteger_ :: Int -> Integer_ -> String
showInteger_ n (Integer_ (Just sign) unsignedInteger) = makeIndent n ++ "Integer_\n" ++ showSign (n + 1) sign ++ showUnsignedInteger (n + 1) unsignedInteger 
showInteger_ n (Integer_ Nothing unsignedInteger) = makeIndent n ++ "Integer_\n" ++ showUnsignedInteger (n + 1) unsignedInteger 
showSign :: Int -> Sign -> String
showSign n (Sign tokenInfo) = makeIndent n ++ "Sign -> \"" ++ getToken tokenInfo ++ "\"\n"
showSubprogramDeclarations :: Int -> SubprogramDeclarations -> String
showSubprogramDeclarations n (SubprogramDeclarations subprogramDeclarationList) = makeIndent n ++ "SubprogramDeclaration\n" ++ concatMap (showSubprogramDeclaration (n + 1)) subprogramDeclarationList 
showSubprogramDeclaration :: Int -> SubprogramDeclaration -> String
showSubprogramDeclaration n (SubprogramDeclaration subprogramHead variableDeclaration compoundStatement) = makeIndent n ++ "SubprogramDeclaration\n" ++ showSubprogramHead (n + 1) subprogramHead ++ showVariableDeclaration (n + 1) variableDeclaration ++ showCompoundStatement (n + 1) compoundStatement 
showSubprogramHead :: Int -> SubprogramHead -> String
showSubprogramHead n (SubprogramHead procedureName parameter) = makeIndent n ++ "SubprogramHead\n" ++ showProcedureName (n + 1) procedureName ++ showParameter (n + 1) parameter 
showProcedureName :: Int -> ProcedureName -> String
showProcedureName n (ProcedureName identifier) = makeIndent n ++ "ProcedureName\n" ++ showIdentifier (n + 1) identifier 
showParameter :: Int -> Parameter -> String
showParameter n (Parameter parameterSequenceList) = makeIndent n ++ "Parameter\n" ++ concatMap (showParameterSequence (n + 1)) parameterSequenceList 
showParameterSequence :: Int -> ParameterSequence -> String
showParameterSequence n (ParameterSequence parameterSequence' parameterSequence'List) = makeIndent n ++ "ParameterSequence\n" ++ showParameterSequence' (n + 1) parameterSequence' ++ concatMap (showParameterSequence' (n + 1)) parameterSequence'List 
showParameterSequence' :: Int -> ParameterSequence' -> String
showParameterSequence' n (ParameterSequence' parameterNameSequence standardType) = makeIndent n ++ "ParameterSequence'\n" ++ showParameterNameSequence (n + 1) parameterNameSequence ++ showStandardType (n + 1) standardType 
showParameterNameSequence :: Int -> ParameterNameSequence -> String
showParameterNameSequence n (ParameterNameSequence parameterName parameterNameList) = makeIndent n ++ "ParameterNameSequence\n" ++ showParameterName (n + 1) parameterName ++ concatMap (showParameterName (n + 1))  parameterNameList 
showParameterName :: Int -> ParameterName -> String
showParameterName n (ParameterName identifier) = makeIndent n ++ "ParameterName\n" ++ showIdentifier (n + 1) identifier 
showCompoundStatement :: Int -> CompoundStatement -> String
showCompoundStatement n (CompoundStatement statementSequence) = makeIndent n ++ "CompoundStatement\n" ++ showStatementSequence (n + 1) statementSequence 
showStatementSequence :: Int -> StatementSequence -> String
showStatementSequence n (StatementSequence statement statementList) = makeIndent n ++ showStatement (n + 1) statement ++ concatMap (showStatement (n + 1)) statementList 
showStatement :: Int -> Statement -> String
showStatement n (Basic basicStatement) = makeIndent n ++ "Statement\n" ++ showBasicStatement (n + 1) basicStatement 
showStatement n (Branch ifStatement) = makeIndent n ++ "Statement\n" ++ showIfStatement (n + 1) ifStatement 
showStatement n (Repeat whileStatement) = makeIndent n ++ "Statement\n" ++ showWhileStatement (n + 1) whileStatement 
showIfStatement :: Int -> IfStatement -> String
showIfStatement n (IfStatement expression compoundStatement elseStatement) = makeIndent n ++ "showIfStatement\n" ++ showExpression (n + 1) expression ++ showCompoundStatement (n + 1) compoundStatement ++ showElseStatement (n + 1) elseStatement 
showElseStatement :: Int -> ElseStatement -> String
showElseStatement n (ElseStatement (Just compoundStatement)) = makeIndent n ++ "ElseStatement\n" ++ showCompoundStatement (n + 1) compoundStatement 
showElseStatement n (ElseStatement Nothing) = makeIndent n ++ "ElseStatement\n"
showWhileStatement :: Int -> WhileStatement -> String
showWhileStatement n (WhileStatement expression compoundStatement) = makeIndent n ++ "WhileStatement\n" ++ showExpression (n + 1) expression ++ showCompoundStatement (n + 1) compoundStatement 
showBasicStatement :: Int -> BasicStatemet -> String
showBasicStatement n (Assign assignStatement) = makeIndent n ++ "BasicStatement\n" ++ showAssignStatement (n + 1) assignStatement 
showBasicStatement n (ProcedureCall procedureCallStatement) = makeIndent n ++ "BasicStatement\n" ++ showProcedureCallStatement (n + 1) procedureCallStatement 
showBasicStatement n (IO ioStatement) = makeIndent n ++ "BasicStatement\n" ++ showIOStatement (n + 1) ioStatement 
showBasicStatement n (Compound compoundStatement) = makeIndent n ++ "BasicStatement\n" ++ showCompoundStatement (n + 1) compoundStatement 
showAssignStatement :: Int -> AssignStatement -> String
showAssignStatement n (AssignStatement leftSide expression) = makeIndent n ++ "AssignStatement\n" ++ showLeftSide (n + 1) leftSide ++ showExpression (n + 1) expression 
showLeftSide :: Int -> LeftSide -> String
showLeftSide n (LeftSide variable) = makeIndent n ++ "LeftSide\n" ++ showVariable (n + 1) variable 
showVariable :: Int -> Variable -> String
showVariable n (Pure purevariable) = makeIndent n ++ "Variable\n" ++ showPureVariable (n + 1) purevariable 
showVariable n (Indexed indexdVarible) = makeIndent n ++ "Variable\n" ++ showIndexedVariable (n + 1) indexdVarible 
showPureVariable :: Int -> PureVariable -> String
showPureVariable n (PureVariable variableName) = makeIndent n ++ "PureVariable\n" ++ showVariableName (n + 1) variableName 
showIndexedVariable :: Int -> IndexedVariable -> String
showIndexedVariable n (IndexedVariable variableName index) = makeIndent n ++ "IndexedVariable\n" ++ showVariableName (n + 1) variableName ++ showIndex (n + 1) index 
showIndex :: Int -> Index -> String
showIndex n (Index expression) = makeIndent n ++ "Index\n" ++ showExpression (n + 1) expression 
showProcedureCallStatement :: Int -> ProcedureCallStatement -> String
showProcedureCallStatement n (ProcedureCallStatement procedureName (Just expressionSequence)) = makeIndent n ++ "ProcedureCallStatement\n" ++ showProcedureName (n + 1) procedureName ++ showExpressionSequence (n + 1) expressionSequence 
showProcedureCallStatement n (ProcedureCallStatement procedureName Nothing) = makeIndent n ++ "ProcedureCallStatement\n" ++ showProcedureName (n + 1) procedureName 
showExpressionSequence :: Int -> ExpressionSequence -> String
showExpressionSequence n (ExpressionSequence expression expressionList) = makeIndent n ++ "ExpressionSequence\n" ++ showExpression (n + 1) expression ++ concatMap (showExpression (n + 1)) expressionList 
showExpression :: Int -> Expression -> String
showExpression n (Expression simpleExpression (Just relationalOperation)) = makeIndent n ++ "Expression\n" ++ showSimpleExpression (n + 1) simpleExpression ++ showRelationalOperation (n + 1) relationalOperation 
showExpression n (Expression simpleExpression Nothing) = makeIndent n ++ "Expression\n" ++ showSimpleExpression (n + 1) simpleExpression 
showRelationalOperation :: Int -> RelationalOperation -> String
showRelationalOperation n (RelationalOperation relationalOperator simpleExpression) = makeIndent n ++ "RelationalOperation\n" ++ showRelationalOperator (n + 1) relationalOperator ++ showSimpleExpression (n + 1) simpleExpression 
showSimpleExpression :: Int -> SimpleExpression -> String
showSimpleExpression n (SimpleExpression (Just sign) term additionalOperationList) = makeIndent n ++ "SimpleExpression\n" ++ showSign (n + 1) sign ++ showTerm (n + 1) term ++ concatMap (showAdditionalOperation (n + 1)) additionalOperationList 
showSimpleExpression n (SimpleExpression Nothing term additionalOperationList) = makeIndent n ++ "SimpleExpression\n" ++ showTerm (n + 1) term ++ concatMap (showAdditionalOperation (n + 1)) additionalOperationList 
showAdditionalOperation :: Int -> AdditionalOperation -> String
showAdditionalOperation n (AdditionalOperation additionalOperator term) = makeIndent n ++ "AdditionalOperation\n" ++ showAdditionalOperator (n + 1) additionalOperator ++ showTerm (n + 1) term
showTerm :: Int -> Term -> String
showTerm n (Term factor multiplicativeOperationList) = makeIndent n ++ "Term\n" ++ showFactor (n + 1) factor ++ concatMap (showMultiplicativeOperation (n + 1)) multiplicativeOperationList 
showMultiplicativeOperation :: Int -> MultiplicativeOperation -> String
showMultiplicativeOperation n (MultiplicativeOperation multiplicativeOperator factor) = makeIndent n ++ "MultiplicativeOperation\n" ++ showMultiplicativeOperator (n + 1) multiplicativeOperator ++ showFactor (n + 1) factor 
showFactor :: Int -> Factor -> String
showFactor n (VariableReference variable) = makeIndent n ++ "Factor\n" ++ showVariable (n + 1) variable 
showFactor n (ConstantReference constant) = makeIndent n ++ "Factor\n" ++ showConstant (n + 1) constant 
showFactor n (Recursion expression) = makeIndent n ++ "Factor\n" ++ showExpression (n + 1) expression 
showFactor n (Negation factor) = makeIndent n ++ "Factor\n" ++ showFactor (n + 1) factor
showRelationalOperator :: Int -> RelationalOperator -> String
showRelationalOperator n (RelationalOperator tokenInfo) = makeIndent n ++ "RelationalOperator -> \"" ++ getToken tokenInfo ++ "\"\n"
showAdditionalOperator :: Int -> AdditionalOperator -> String
showAdditionalOperator n (AdditionalOperator tokenInfo) = makeIndent n ++ "AdditionalOperator -> \"" ++ getToken tokenInfo ++ "\"\n"
showMultiplicativeOperator :: Int -> MultiplicativeOperator -> String
showMultiplicativeOperator n (MultiplicativeOperator tokenInfo) = makeIndent n ++ "Multiplicative -> \"" ++ getToken tokenInfo ++ "\"\n"
showIOStatement :: Int -> IOStatement -> String
showIOStatement n (InputStatement (Just variableSequence)) = makeIndent n ++ "IOStatement\n" ++ showVariableSequence (n + 1) variableSequence 
showIOStatement n (InputStatement Nothing) = makeIndent n ++ "IOStatement\n"
showIOStatement n (OutputStatement (Just expressionSequence)) = makeIndent n ++ "IOStatement\n" ++ showExpressionSequence (n + 1) expressionSequence 
showIOStatement n (OutputStatement Nothing) = makeIndent n ++ "IOStatement\n"
showVariableSequence :: Int -> VariableSequence -> String
showVariableSequence n (VariableSequence variable variableList) = makeIndent n ++ "VariableSequence\n" ++ showVariable (n + 1) variable ++ concatMap (showVariable (n + 1)) variableList 
showConstant :: Int -> Constant -> String
showConstant n (IntegerLiteral unsignedInteger) = makeIndent n ++ "Constant\n" ++ showUnsignedInteger (n + 1) unsignedInteger 
showConstant n (StringLiteral string) = makeIndent n ++ "Constant\n" ++ showString_ (n + 1) string 
showConstant n (BooleanLiteral boolean) = makeIndent n ++ "Constant\n" ++ showBoolean (n + 1) boolean 
showUnsignedInteger :: Int -> UnsignedInteger -> String
showUnsignedInteger n (UnsignedInteger tokenInfo) = makeIndent n ++ "UnsignedInteger -> \"" ++ getToken tokenInfo ++ "\"\n"
showString_ :: Int -> String_ -> String
showString_ n (String_ tokenInfo) = makeIndent n ++ "String -> \"" ++ getToken tokenInfo ++ "\"\n"
showBoolean :: Int -> Boolean -> String
showBoolean n (Boolean tokenInfo) = makeIndent n ++ "Boolean -> \"" ++ getToken tokenInfo ++ "\"\n"
showIdentifier :: Int -> Identifier -> String
showIdentifier n (Identifier tokenInfo) = makeIndent n ++ "Identifier -> \"" ++ getToken tokenInfo ++ "\"\n"