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

import AST (AST, Value(..))
import Types.Checker ((|-), (<:))
import Types.Results (TypeJudgementResult(..), EmperorType(..))
import Types.Environment (newTypeEnvironment, TypeEnvironment, get)
import Types.PreludeTypes (eqable, PreludeType)

-- | Class describing constructs which may be assigned a type.
class Typable a where
    -- | Obtain the type of a given construct from a fresh environment.
    judge :: a -> TypeJudgementResult
    judge = (newTypeEnvironment |>)

    -- | Judge the type of a given construct under a particular typing 
    -- environment.
    (|>) :: TypeEnvironment -> a -> TypeJudgementResult
    infixl 1 |>

instance Typable Expr where
    g |> (Value v)                  = g |> v
    g |> (Neg e)                    = let t = g |> e in 
                                        assert (g |- (t <: Real)) ("Could not unify Bool and " ++ show t) t
    g |> (Add e1 e2)                = assertExpr Real e1 e2
    g |> (Subtract e1 e2)           = assertExpr Real e1 e2
    g |> (Multiply e1 e2)           = assertExpr Real e1 e2
    g |> (Divide e1 e2)             = assertExpr Real e1 e2
    g |> (Modulo e1 e2)             = assertExpr Real e1 e2
    g |> (Less e1 e2)               = assertExpr Real e1 e2
    g |> (LessOrEqual e1 e2)        = assertExpr Real e1 e2
    g |> (Greater e1 e2)            = assertExpr Real e1 e2
    g |> (GreaterOrEqual e1 e2)     = assertExpr Real e1 e2
    g |> (Equal e1 e2)              = assertEquality e1 e2
    g |> (NotEqual e1 e2)           = assertEquality e1 e2
    g |> (Not e)                    = let t = g |> e in
                                        assert (t == Bool) ("Could not unify Bool and " ++ show t) t
    g |> (AndStrict e1 e2)          = assertExpr Real e1 e2
    g |> (AndLazy e1 e2)            = assertExpr Real e1 e2
    g |> (OrStrict e1 e2)           = assertExpr Real e1 e2
    g |> (OrLazy e1 e2)             = assertExpr Real e1 e2
    g |> (Implies e1 e2)            = assertExpr Real e1 e2
    g |> (Xor e1 e2)                = assertExpr Real e1 e2
    g |> (ShiftLeft e1 e2)          = assertExpr Int e1 e2 
    g |> (ShiftRight e1 e2)         = assertExpr Int e1 e2 
    g |> (ShiftRightSameSign e1 e2) = assertExpr Int e1 e2 
    g |> (Set (e:es))               = let t = g |> e in
                                        assert (all (== t) ((g |>) <$> es)) "All elements of a set must have the same type" (ESet t)
    g |> (List (e:es))              = let t = g |> e in
                                        assert (all (== t) ((g |>) <$> es)) "All elements of a set must have the same type" (EList t)
    g |> (Tuple es)                 = ETuple $ (g |>) <$> es
    g |> (PureCallExpr p)           = EFunction Pure $
    g |> (ImpureCallExpr i)         = EFunction Impure $

assertExpr :: Typable a => Typable b => EmperorType -> a -> b -> EmperorType
assertExpr t e1 e2 = let t1 = g |> e1 in 
                        let t2 = g |> e2 in 
                            assert (g |- (t1 <: t) && g |- (t2 <: t)) ("Could not unify " ++ show t1 ++ ", " ++ show t2 ++ " <: " ++ show t) t1

assertEquality :: Typable a => Typable b => a -> b -> EmperorType
assertEquality e1 e2 = let t1 = g |> e1 in
                        let t2 = g |> e2 in
                            assert (all (g |- ) [ t1 <: equable, t2 <: t1, t2 <: t1])
                            (g |- (t1 <: equable) && g |- (t2 <: t1) && g |- (t2 <: t1)) "Could not unify"

instance Typable Value where
    _ |> (Integer _) = Valid IntP
    _ |> (Real _) = Valid RealP
    _ |> (Char _) = Valid CharP
    _ |> (Bool _) = Valid BoolP
    -- _ |> (String _) = Valid $ EList CharP
    g |> (IdentV i) = get i g

assert :: Bool -> TypeJudgementResult -> String -> TypeJudgementResult
assert False _ s = Invalid s
assert _ j _     = j
