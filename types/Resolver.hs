module Resolver ((|>), judge, Typable) where

import AST (AST)
-- import Checker ((|-), (<:))
import Results (TypeJudgementResult(..))
import TypeEnvironment (newTypeEnvironment, TypeEnvironment)

class Typable a where
    judge :: a -> TypeJudgementResult a
    judge = (newTypeEnvironment |>)

    (|>) :: TypeEnvironment -> a -> TypeJudgementResult a
    infixl 1 |>

instance Typable AST where
    _ |> _ = Invalid "Type-checking has not been implemented yet..."