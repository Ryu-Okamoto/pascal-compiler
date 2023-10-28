module Src.Checker.DType where

data DType = DInteger | DIntegerArray |
             DChar    | DCharArray    |
             DBoolean | DBooleanArray    
    deriving ( Show, Eq, Ord )