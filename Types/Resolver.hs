{-|
Module      : RResolver
Description : Type resolver for emperor
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

This module defines typing judgements for the AST. Types are found and checked 
for compatibility.
-}
module Types.Resolver ((|>), judge, Typable) where

import AST (AST)
-- import Checker ((|-), (<:))
import Types.Results (TypeJudgementResult(..))
import Types.Environment (newTypeEnvironment, TypeEnvironment)

-- | Class describing constructs which may be assigned a type.
class Typable a where
    -- | Obtain the type of a given construct from a fresh environment.
    judge :: a -> TypeJudgementResult a
    judge = (newTypeEnvironment |>)

    -- | Judge the type of a given construct under a particular typing 
    -- environment.
    (|>) :: TypeEnvironment -> a -> TypeJudgementResult a
    infixl 1 |>

instance Typable AST where
    _ |> _ = Invalid "Type-checking has not been implemented yet..."