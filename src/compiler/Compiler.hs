module Src.Compiler where

{-
    TODO:
     - VariableTable, ProcedureTable の改造
      - VariableTable
       - 変数のサイズとオフセット情報を持つようにする
        - 変数領域確保、参照の際に必要
      - ProcedureTable
       - ラベル情報を持つようにする
     - コンパイル時にどうしても必要な状態情報の整理
      - if, while 等の制御構文に対するラベル
      - 定数の管理、ConstantTable 作った方が早そう
-}

