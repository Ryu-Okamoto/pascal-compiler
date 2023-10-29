module Src.Parser.ParseMonad ( Parse (..) ) where

import Src.Synonym ( LineNumber )

data Parse a = Parse a | SyntaxError LineNumber
instance Functor Parse where
    fmap :: (a -> b) -> Parse a -> Parse b
    fmap f (Parse x) = Parse (f x)
    fmap _ (SyntaxError lineNumber) = SyntaxError lineNumber
instance Applicative Parse where
    pure :: a -> Parse a
    pure = Parse
    (<*>) :: Parse (a -> b) -> Parse a -> Parse b
    (<*>) (Parse f) (Parse x) = Parse (f x)
    (<*>) _ (SyntaxError lineNumber) = SyntaxError lineNumber
    (<*>) (SyntaxError lineNumber) _ = SyntaxError lineNumber
instance Monad Parse where
    (>>=) :: Parse a -> (a -> Parse b) -> Parse b
    (>>=) (Parse x) f = f x
    (>>=) (SyntaxError lineNumber) _ = SyntaxError lineNumber
