module Lexer where

type InputFilePath = FilePath
type OutputFilePath = FilePath

type SourceCode = String

type Token = String
type TokenType = String
type TokenID = String
type LineNumber = Int
data TokenInfo = TokenInfo { getToken :: Token, getTokenType :: TokenType, getTokenID :: TokenID, getLineNumber :: LineNumber } deriving (Show)

data State = Q0 | Q1 | Q2 | Q3 | Q4 | Q5 | Q6 | Q7 | Q8 | Q9 | Q10 | Q11 | Q12 | Q13 | Q14 | QF deriving (Eq)

lex :: InputFilePath -> OutputFilePath -> IO ()
lex inputFilePath outputFilePath = do
    sourceCodeLines <- lines <$> readFile inputFilePath
    let tokenInfos = processLine 1 sourceCodeLines
    let output = map (\tokenInfo -> getToken tokenInfo ++ "\t" ++ getTokenType tokenInfo ++ "\t" ++ getTokenID tokenInfo ++ "\t" ++ show (getLineNumber tokenInfo)) tokenInfos
    writeFile outputFilePath $ unlines output

processLine :: LineNumber -> [SourceCode] -> [TokenInfo]
processLine lineNumber (h:t) = tokenInfos ++ processLine (lineNumber + 1) t
    where tokenInfos = map (toInfo lineNumber) (splitToken h)
          toInfo :: LineNumber -> Token -> TokenInfo
          toInfo lineNumber token = uncurry (TokenInfo token) tokenTupple lineNumber
              where tokenTupple = associateToken token
processLine _ [] = []

associateToken :: Token -> (TokenType, TokenID)
associateToken token
    | token == "and" = ("SAND", "0")
    | token == "array" = ("SARRAY", "1")
    | token == "begin" = ("SBEGIN", "2")
    | token == "boolean" = ("SBOOLEAN", "3")
    | token == "char" = ("SCHAR", "4")
    | token == "div" = ("SDIV", "5")
    | token == "/" = ("SDIV", "5")
    | token == "do" = ("SDO", "6")
    | token == "else" = ("SELSE", "7")
    | token == "end" = ("SEND", "8")
    | token == "false" = ("SFALSE", "9")
    | token == "if" = ("SIF", "10")
    | token == "integer" = ("SINTEGER", "11")
    | token == "mod" = ("SMOD", "12")
    | token == "not" = ("SNOT", "13")
    | token == "of" = ("SOF", "14")
    | token == "or" = ("SOR", "15")
    | token == "procedure" = ("SPROCEDURE", "16")
    | token == "program" = ("SPROGRAM", "17")
    | token == "readln" = ("SREADLN", "18")
    | token == "then" = ("STHEN", "19")
    | token == "true" = ("STRUE", "20")
    | token == "var" = ("SVAR", "21")
    | token == "while" = ("SWHILE", "22")
    | token == "writeln" = ("SWRITELN", "23")
    | token == "=" = ("SEQUAL", "24")
    | token == "<>" = ("SNOTEQUAL", "25")
    | token == "<" = ("SLESS", "26")
    | token == "<=" = ("SLESSEQUAL", "27")
    | token == ">=" = ("SGREATEQUAL", "28")
    | token == ">" = ("SGREAT", "29")
    | token == "+" = ("SPLUS", "30")
    | token == "-" = ("SMINUS", "31")
    | token == "*" = ("SSTAR", "32")
    | token == "(" = ("SLPAREN", "33")
    | token == ")" = ("SRPAREN", "34")
    | token == "[" = ("SLBRACKET", "35")
    | token == "]" = ("SRBRACKET", "36")
    | token == ";" = ("SSEMICOLON", "37")
    | token == ":" = ("SCOLON", "38")
    | token == ".." = ("SRANGE", "39")
    | token == ":=" = ("SASSIGN", "40")
    | token == "," = ("SCOMMA", "41")
    | token == "." = ("SDOT", "42")
    | isString token = ("SSTRING", "45")
    | isNumber token = ("SCONSTANT", "44")
    | otherwise = ("SIDENTIFIER", "43")
    where isString :: String -> Bool
          isString (h:t) = h == '\''
          isString [] = False
          isNumber :: String -> Bool
          isNumber (h:t) = h `elem` ['0' .. '9']
          isNumber [] = False

splitToken :: SourceCode -> [Token]
splitToken sourceCode
    | null sourceCode = []
    | otherwise = firstToken : splitToken rest
    where (firstToken, rest) = clipFirstToken Q0 sourceCode

clipFirstToken :: State -> SourceCode -> (Token, SourceCode)
clipFirstToken state (h:t)
    | (h `elem` [' ', '\t']) && (state == Q0) = clipFirstToken state t
    | nextState == QF = ([], h:t)
    | otherwise = (h:restToken, rest)
    where nextState = transition (state, h)
          (restToken, rest) = clipFirstToken nextState t
clipFirstToken _ [] = ([], [])

transition :: (State, Char) -> State
transition (Q0, a)
    | '1' <= a && a <= '9' = Q1
    | 'a' <= a && a <= 'z' || 'A' <= a && a <= 'Z' = Q2
    | a == '=' || a == '+' || a == '-' || a == '*' || a == '/' || a == '(' || a == ')' || a == '[' || a == ']' || a == ';' || a == ',' = Q3
    | a == '>' || a == ':' = Q4
    | a == '.' = Q6
    | a == '\'' = Q8
    | a == '<' = Q10
    | a == '{' = Q13
transition (Q1, a)
    | '0' <= a && a <= '9' = Q1
transition (Q2, a)
    | 'a' <= a && a <= 'z' || 'A' <= a && a <= 'Z' || '0' <= a && a <= '9' = Q2
transition (Q4, a)
    | a == '=' = Q5
transition (Q6, a)
    | a == '.' = Q7
transition (Q8, a)
    | a == '\'' || a == '\n' = Q9
    | otherwise = Q8
transition (Q10, a)
    | a == '>' = Q11
    | a == '=' = Q12
transition (Q13, a)
    | a == '}' = Q14
    | otherwise = Q13
transition (_, _) = QF