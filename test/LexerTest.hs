module Test.LexerTest where

import Test.Hspec ( Spec, describe, it, shouldBe, hspec )

import Src.Token ( Token (..), tsToTokens )
import Src.Lexer.Lexer ( run )

run :: IO ()
run = do
    pas01 <- readFile "./data/pas/normal01.pas"; ts01 <- readFile "./data/ts/normal01.ts"
    pas02 <- readFile "./data/pas/normal02.pas"; ts02 <- readFile "./data/ts/normal02.ts"
    pas03 <- readFile "./data/pas/normal03.pas"; ts03 <- readFile "./data/ts/normal03.ts"
    pas04 <- readFile "./data/pas/normal04.pas"; ts04 <- readFile "./data/ts/normal04.ts"
    pas05 <- readFile "./data/pas/normal05.pas"; ts05 <- readFile "./data/ts/normal05.ts"
    pas06 <- readFile "./data/pas/normal06.pas"; ts06 <- readFile "./data/ts/normal06.ts"
    pas07 <- readFile "./data/pas/normal07.pas"; ts07 <- readFile "./data/ts/normal07.ts"
    pas08 <- readFile "./data/pas/normal08.pas"; ts08 <- readFile "./data/ts/normal08.ts"
    pas09 <- readFile "./data/pas/normal09.pas"; ts09 <- readFile "./data/ts/normal09.ts"
    pas10 <- readFile "./data/pas/normal10.pas"; ts10 <- readFile "./data/ts/normal10.ts"
    pas11 <- readFile "./data/pas/normal11.pas"; ts11 <- readFile "./data/ts/normal11.ts"
    pas12 <- readFile "./data/pas/normal12.pas"; ts12 <- readFile "./data/ts/normal12.ts"
    pas13 <- readFile "./data/pas/normal13.pas"; ts13 <- readFile "./data/ts/normal13.ts"
    pas14 <- readFile "./data/pas/normal14.pas"; ts14 <- readFile "./data/ts/normal14.ts"
    pas15 <- readFile "./data/pas/normal15.pas"; ts15 <- readFile "./data/ts/normal15.ts"
    pas16 <- readFile "./data/pas/normal16.pas"; ts16 <- readFile "./data/ts/normal16.ts"
    pas17 <- readFile "./data/pas/normal17.pas"; ts17 <- readFile "./data/ts/normal17.ts"
    pas18 <- readFile "./data/pas/normal18.pas"; ts18 <- readFile "./data/ts/normal18.ts"
    pas19 <- readFile "./data/pas/normal19.pas"; ts19 <- readFile "./data/ts/normal19.ts"
    pas20 <- readFile "./data/pas/normal20.pas"; ts20 <- readFile "./data/ts/normal20.ts"
    hspec $ do
        test "normal01" pas01 ts01
        test "normal02" pas02 ts02
        test "normal03" pas03 ts03
        test "normal04" pas04 ts04
        test "normal05" pas05 ts05
        test "normal06" pas06 ts06
        test "normal07" pas07 ts07
        test "normal08" pas08 ts08
        test "normal09" pas09 ts09
        test "normal10" pas10 ts10
        test "normal11" pas11 ts11
        test "normal12" pas12 ts12
        test "normal13" pas13 ts13
        test "normal14" pas14 ts14
        test "normal15" pas15 ts15
        test "normal16" pas16 ts16
        test "normal17" pas17 ts17
        test "normal18" pas18 ts18
        test "normal19" pas19 ts19
        test "normal20" pas20 ts20
    where
        test :: String -> String -> String -> Spec
        test description source ts = describe description $ it "standard" $ Src.Lexer.Lexer.run source `shouldBe` tsToTokens ts