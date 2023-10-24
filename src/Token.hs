module Src.Token where

import Data.Char ( isNumber, isAlpha )

import Src.Synonym ( LineNumber )

data Token = Token {
    getSSymbol :: String, 
    getSType :: String,
    getSID :: String,
    getSLineNumber :: LineNumber
} deriving ( Show, Eq )

createToken :: String -> LineNumber -> Token
createToken (tokenH:tokenT) lineNumber
    | isNumber tokenH      = Token (tokenH:tokenT) "SCONSTANT" "44" lineNumber
    | tokenH == '\''       = Token (tokenH:tokenT) "SSTRING"   "45" lineNumber
createToken token lineNumber
    | token == "and"       = Token "and"       "SAND"        "0"  lineNumber
    | token == "array"     = Token "array"     "SARRAY"      "1"  lineNumber
    | token == "begin"     = Token "begin"     "SBEGIN"      "2"  lineNumber
    | token == "boolean"   = Token "boolean"   "SBOOLEAN"    "3"  lineNumber
    | token == "char"      = Token "char"      "SCHAR"       "4"  lineNumber
    | token == "div"       = Token "div"       "SDIVD"       "5"  lineNumber
    | token == "/"         = Token "/"         "SDIVD"       "5"  lineNumber
    | token == "do"        = Token "do"        "SDO"         "6"  lineNumber
    | token == "else"      = Token "else"      "SELSE"       "7"  lineNumber
    | token == "end"       = Token "end"       "SEND"        "8"  lineNumber
    | token == "false"     = Token "false"     "SFALSE"      "9"  lineNumber
    | token == "if"        = Token "if"        "SIF"         "10" lineNumber
    | token == "integer"   = Token "integer"   "SINTEGER"    "11" lineNumber
    | token == "mod"       = Token "mod"       "SMOD"        "12" lineNumber
    | token == "not"       = Token "not"       "SNOT"        "13" lineNumber
    | token == "of"        = Token "of"        "SOF"         "14" lineNumber
    | token == "or"        = Token "or"        "SOR"         "15" lineNumber
    | token == "procedure" = Token "procedure" "SPROCEDURE"  "16" lineNumber
    | token == "program"   = Token "program"   "SPROGRAM"    "17" lineNumber
    | token == "readln"    = Token "readln"    "SREADLN"     "18" lineNumber
    | token == "then"      = Token "then"      "STHEN"       "19" lineNumber
    | token == "true"      = Token "true"      "STRUE"       "20" lineNumber
    | token == "var"       = Token "var"       "SVAR"        "21" lineNumber
    | token == "while"     = Token "while"     "SWHILE"      "22" lineNumber
    | token == "writeln"   = Token "writeln"   "SWRITELN"    "23" lineNumber
    | token == "="         = Token "="         "SEQUAL"      "24" lineNumber
    | token == "<>"        = Token "<>"        "SNOTEQUAL"   "25" lineNumber
    | token == "<"         = Token "<"         "SLESS"       "26" lineNumber
    | token == "<="        = Token "<="        "SLESSEQUAL"  "27" lineNumber
    | token == ">="        = Token ">="        "SGREATEQUAL" "28" lineNumber
    | token == ">"         = Token ">"         "SGREAT"      "29" lineNumber
    | token == "+"         = Token "+"         "SPLUS"       "30" lineNumber
    | token == "-"         = Token "-"         "SMINUS"      "31" lineNumber
    | token == "*"         = Token "*"         "SSTAR"       "32" lineNumber
    | token == "("         = Token "("         "SLPAREN"     "33" lineNumber
    | token == ")"         = Token ")"         "SRPAREN"     "34" lineNumber
    | token == "["         = Token "["         "SLBRACKET"   "35" lineNumber
    | token == "]"         = Token "]"         "SRBRACKET"   "36" lineNumber
    | token == ";"         = Token ";"         "SSEMICOLON"  "37" lineNumber
    | token == ":"         = Token ":"         "SCOLON"      "38" lineNumber
    | token == ".."        = Token ".."        "SRANGE"      "39" lineNumber
    | token == ":="        = Token ":="        "SASSIGN"     "40" lineNumber
    | token == ","         = Token ","         "SCOMMA"      "41" lineNumber
    | token == "."         = Token "."         "SDOT"        "42" lineNumber
    | otherwise            = Token token       "SIDENTIFIER" "43" lineNumber 