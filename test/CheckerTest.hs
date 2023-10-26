module Test.CheckerTest where

import Test.Hspec ( Spec, describe, it, shouldBe, hspec )
import Src.Token ( Token )
import Src.AST ( AST )
import Src.Parser.Parser ( run, Parse (..) )
import Src.Checker.Checker ( run, Check (..) )

import Test.LexerTest ( tsToTokens )

run :: IO()
run = do
    runNormal
    runSemerr
    runSynerr

runNormal :: IO ()
runNormal = do
    normal01 <- readFile "./data/ts/normal01.ts"; let tsNormal01 = tsToTokens normal01;
    normal02 <- readFile "./data/ts/normal02.ts"; let tsNormal02 = tsToTokens normal02;
    normal03 <- readFile "./data/ts/normal03.ts"; let tsNormal03 = tsToTokens normal03;
    normal04 <- readFile "./data/ts/normal04.ts"; let tsNormal04 = tsToTokens normal04;
    normal05 <- readFile "./data/ts/normal05.ts"; let tsNormal05 = tsToTokens normal05;
    normal06 <- readFile "./data/ts/normal06.ts"; let tsNormal06 = tsToTokens normal06;
    normal07 <- readFile "./data/ts/normal07.ts"; let tsNormal07 = tsToTokens normal07;
    normal08 <- readFile "./data/ts/normal08.ts"; let tsNormal08 = tsToTokens normal08;
    normal09 <- readFile "./data/ts/normal09.ts"; let tsNormal09 = tsToTokens normal09;
    normal10 <- readFile "./data/ts/normal10.ts"; let tsNormal10 = tsToTokens normal10;
    normal11 <- readFile "./data/ts/normal11.ts"; let tsNormal11 = tsToTokens normal11;
    normal12 <- readFile "./data/ts/normal12.ts"; let tsNormal12 = tsToTokens normal12;
    normal13 <- readFile "./data/ts/normal13.ts"; let tsNormal13 = tsToTokens normal13;
    normal14 <- readFile "./data/ts/normal14.ts"; let tsNormal14 = tsToTokens normal14;
    normal15 <- readFile "./data/ts/normal15.ts"; let tsNormal15 = tsToTokens normal15;
    normal16 <- readFile "./data/ts/normal16.ts"; let tsNormal16 = tsToTokens normal16;
    normal17 <- readFile "./data/ts/normal17.ts"; let tsNormal17 = tsToTokens normal17;
    normal18 <- readFile "./data/ts/normal18.ts"; let tsNormal18 = tsToTokens normal18;
    normal19 <- readFile "./data/ts/normal19.ts"; let tsNormal19 = tsToTokens normal19;
    normal20 <- readFile "./data/ts/normal20.ts"; let tsNormal20 = tsToTokens normal20;
    hspec $ do
        test "normal01" tsNormal01 ""
        test "normal02" tsNormal02 ""
        test "normal03" tsNormal03 ""
        test "normal04" tsNormal04 ""
        test "normal05" tsNormal05 ""
        test "normal06" tsNormal06 ""
        test "normal07" tsNormal07 ""
        test "normal08" tsNormal08 ""
        test "normal09" tsNormal09 ""
        test "normal10" tsNormal10 ""
        test "normal11" tsNormal11 ""
        test "normal12" tsNormal12 ""
        test "normal13" tsNormal13 ""
        test "normal14" tsNormal14 ""
        test "normal15" tsNormal15 ""
        test "normal16" tsNormal16 ""
        test "normal17" tsNormal17 ""
        test "normal18" tsNormal18 ""
        test "normal19" tsNormal19 ""
        test "normal20" tsNormal20 ""

runSemerr :: IO ()
runSemerr = do
    semerr01 <- readFile "./data/ts/semerr01.ts"; let tsSemerr01 = tsToTokens semerr01;
    semerr02 <- readFile "./data/ts/semerr02.ts"; let tsSemerr02 = tsToTokens semerr02;
    semerr03 <- readFile "./data/ts/semerr03.ts"; let tsSemerr03 = tsToTokens semerr03;
    semerr04 <- readFile "./data/ts/semerr04.ts"; let tsSemerr04 = tsToTokens semerr04;
    semerr05 <- readFile "./data/ts/semerr05.ts"; let tsSemerr05 = tsToTokens semerr05;
    semerr06 <- readFile "./data/ts/semerr06.ts"; let tsSemerr06 = tsToTokens semerr06;
    semerr07 <- readFile "./data/ts/semerr07.ts"; let tsSemerr07 = tsToTokens semerr07;
    semerr08 <- readFile "./data/ts/semerr08.ts"; let tsSemerr08 = tsToTokens semerr08;
    hspec $ do
        test "synerr01" tsSemerr01 ""
        test "synerr02" tsSemerr02 ""
        test "synerr03" tsSemerr03 ""
        test "synerr04" tsSemerr04 ""
        test "synerr05" tsSemerr05 ""
        test "synerr06" tsSemerr06 ""
        test "synerr07" tsSemerr07 ""
        test "synerr08" tsSemerr08 ""

runSynerr :: IO ()
runSynerr = do
    synerr01 <- readFile "./data/ts/synerr01.ts"; let tsSynerr01 = tsToTokens synerr01;
    synerr02 <- readFile "./data/ts/synerr02.ts"; let tsSynerr02 = tsToTokens synerr02;
    synerr03 <- readFile "./data/ts/synerr03.ts"; let tsSynerr03 = tsToTokens synerr03;
    synerr04 <- readFile "./data/ts/synerr04.ts"; let tsSynerr04 = tsToTokens synerr04;
    synerr05 <- readFile "./data/ts/synerr05.ts"; let tsSynerr05 = tsToTokens synerr05;
    synerr06 <- readFile "./data/ts/synerr06.ts"; let tsSynerr06 = tsToTokens synerr06;
    synerr07 <- readFile "./data/ts/synerr07.ts"; let tsSynerr07 = tsToTokens synerr07;
    synerr08 <- readFile "./data/ts/synerr08.ts"; let tsSynerr08 = tsToTokens synerr08;
    hspec $ do
        test "synerr01" tsSynerr01 "1"
        test "synerr02" tsSynerr02 "3"
        test "synerr03" tsSynerr03 "8"
        test "synerr04" tsSynerr04 "10"
        test "synerr05" tsSynerr05 "11"
        test "synerr06" tsSynerr06 "13"
        test "synerr07" tsSynerr07 "30"
        test "synerr08" tsSynerr08 "31"

test :: String -> [Token] -> String -> Spec
test description tokens expected = describe description $ it "standard" $ result `shouldBe` expected
    where
        result = let res1 = Src.Parser.Parser.run tokens in 
                    case res1 of
                        (SyntaxError lineNumber) -> lineNumber
                        (Parse ast) -> let res2 = Src.Checker.Checker.run ast in
                                            case res2 of
                                            (SemanticError lineNumber) -> lineNumber
                                            _ -> ""