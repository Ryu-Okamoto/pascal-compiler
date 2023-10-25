# サブセット Pascal コンパイラ
Pascal のサブセット言語の Haskell 実装コンパイラです。  
アセンブラとして CASL II を吐き出します。  
文法の定義は [辻野 嘉宏、「コンパイラ（第2版）」](https://www.amazon.co.jp/コンパイラ-第2版-辻野-嘉宏/dp/4274224724) の付録にあるものにしたがいます。

## 実行方法
[cabal](https://www.haskell.org/cabal/) のインストールが必要です。  
init を叩くと必要な外部パッケージがインストールされます。  
テストを含め実行は ghci のインタプリタから行います。

例）Parser テストの実行
~~~
$ ghci
ghci > :l Test.ParserTest
ghci > Test.ParserTest.run
...
ghci > :q // ghci の終了
~~~