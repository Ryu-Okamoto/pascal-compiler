module Src.Lexer.Lexer ( run ) where

import Data.Char ( isNumber, isAlpha, isAlphaNum )

import Src.Synonym ( SourceCode, SourceLine, LineNumber )
import Src.Token ( Token, createToken )

{-
    実装方針：
     - 状態遷移機械（DFA）を模倣
      - 形式定義をそのまま関数化し適用
-}

run :: SourceCode -> [Token]
run source = loopForLines (lines source) "1"
    where
        loopForLines :: [SourceLine] -> LineNumber -> [Token]
        loopForLines [] _ = []
        loopForLines (linesH:linesT) lineNumber = splitToTokens linesH lineNumber ++ loopForLines linesT incremented
            where
                incremented = show ((read lineNumber :: Int) + 1)

splitToTokens :: SourceLine -> LineNumber -> [Token]
splitToTokens [] _ = []
splitToTokens source lineNumber
    | firstToken == "" = splitToTokens rest lineNumber
    | otherwise = createToken firstToken lineNumber : splitToTokens rest lineNumber
    where
        (_, firstToken, rest) = extractFirstToken (State0, [], source)
        extractFirstToken :: (DFAState, String, SourceLine) -> (DFAState, String, SourceLine)
        extractFirstToken (StateF, extracted, rest) = (State0, init extracted, last extracted : rest)
        extractFirstToken (State0, extracted, ' ':rest) = extractFirstToken (State0, extracted, rest)
        extractFirstToken (State0, extracted, '\n':rest) = extractFirstToken (State0, extracted, rest)
        extractFirstToken (State13, _, restH:restT) = extractFirstToken (dfaTransition (State13, restH), [], restT)
        extractFirstToken (state, extracted, []) = (State0, extracted, [])
        extractFirstToken (state, extracted, restH:restT) = extractFirstToken (dfaTransition (state, restH), extracted ++ [restH], restT)

data DFAState = State0  | State1  | State2  | State3  |
                State4  | State5  | State6  | State7  |
                State8  | State9  | State10 | State11 |
                State12 | State13 | State14 | StateF

dfaTransition :: (DFAState, Char) -> DFAState
dfaTransition (State0, a)
    | isNumber a = State1
    | isAlpha a = State2
    | a `elem` ['=', '+', '-', '*', '/', '(', ')', '[', ']', ';', ','] = State3
    | a `elem` ['>', ':'] = State4
    | a == '.' = State6
    | a == '\'' = State8
    | a == '<' = State10
    | a == '{' = State13
dfaTransition (State1, a)
    | isNumber a = State1
dfaTransition (State2, a)
    | isAlphaNum a = State2
dfaTransition (State4, a)
    | a == '=' = State5
dfaTransition (State6, a)
    | a == '.' = State7
dfaTransition (State8, a)
    | a `elem` ['\'', '\n'] = State9
    | otherwise = State8
dfaTransition (State10, a)
    | a == '>' = State11
    | a == '=' = State12
dfaTransition (State13, a)
    | a == '}' = State14
    | otherwise = State13
dfaTransition (_, _) = StateF