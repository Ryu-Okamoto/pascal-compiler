module Test.LexerTest where

import Test.Hspec ( Spec, describe, it, shouldBe, hspec )

import Src.Lexer ( run )
import Src.Token ( Token (Token) )
import Src.Synonym ( SourceCode )

run :: IO ()
run = do
    source01 <- readFile "./data/pas/normal01.pas"
    ts01 <- readFile "./data/ts/normal01.ts"
    hspec $ do
        testNormal01 source01 ts01

tsToTokens :: String -> [Token]
tsToTokens ts = map ((\(s:t:i:l:_) -> Token s t i l) . words) (lines ts)

testNormal01 :: SourceCode -> String -> Spec
testNormal01 source ts = do
    describe "lexer test: normal01" $ do
        it "standard" $ Src.Lexer.run source `shouldBe` expectedTokens
    where
        expectedTokens = tsToTokens ts