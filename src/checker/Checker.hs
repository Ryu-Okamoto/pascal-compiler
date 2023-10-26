module Src.Checker.Checker ( run, Check (..) ) where

import Src.Synonym ( LineNumber )
import Src.AST

data Check a = Check a | SemanticError LineNumber
instance Functor Check where
    fmap :: (a -> b) -> Check a -> Check b
    fmap f (Check x) = Check (f x)
    fmap _ (SemanticError lineNumber) = SemanticError lineNumber
instance Applicative Check where
    pure :: a -> Check a
    pure = Check
    (<*>) :: Check (a -> b) -> Check a -> Check b
    (<*>) (Check f) (Check x) = Check (f x)
    (<*>) _ (SemanticError lineNumber) = SemanticError lineNumber
    (<*>) (SemanticError lineNumber) _ = SemanticError lineNumber
instance Monad Check where
    (>>=) :: Check a -> (a -> Check b) -> Check b
    (>>=) (Check x) f = f x
    (>>=) (SemanticError lineNumber) _ = SemanticError lineNumber

run :: AST -> Check ()
run = undefined