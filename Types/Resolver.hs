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
module Types.Resolver
    ( (|>)
    , judge
    , Typable
    ) where

import AST (Expr(..), Ident(..), Purity(..), Value(..))
import Types.Checker ((<:), (|-))
import Types.Environment (TypeEnvironment, get, newTypeEnvironment, unsafeGet)
import Types.PreludeTypes (PreludeType, eqable)
import Types.Results
    ( EmperorType(..)
    , TypeCheckResult(..)
    , TypeJudgementResult(..)
    , isValid
    )

-- | Class describing constructs which may be assigned a type.
class Typable a
    -- | Obtain the type of a given construct from a fresh environment.
    where
    judge :: a -> TypeJudgementResult
    judge = (newTypeEnvironment |>)
    -- | Judge the type of a given construct under a particular typing 
    -- environment.
    (|>) :: TypeEnvironment -> a -> TypeJudgementResult
    infixl 1 |>

instance Typable Expr where
    g |> (Value v) = g |> v
    g |> (Neg e) =
        let t = g |> e
         in case t of
                Valid t' ->
                    assert
                        ((g |- (t' <: RealP)) == Pass)
                        ("Could not unify Bool and " ++ show t')
                        t
                x -> x
    g |> (Add e1 e2) = assertExpr g RealP e1 e2
    g |> (Subtract e1 e2) = assertExpr g RealP e1 e2
    g |> (Multiply e1 e2) = assertExpr g RealP e1 e2
    g |> (Divide e1 e2) = assertExpr g RealP e1 e2
    g |> (Modulo e1 e2) = assertExpr g RealP e1 e2
    g |> (Less e1 e2) = assertExpr g RealP e1 e2
    g |> (LessOrEqual e1 e2) = assertExpr g RealP e1 e2
    g |> (Greater e1 e2) = assertExpr g RealP e1 e2
    g |> (GreaterOrEqual e1 e2) = assertExpr g RealP e1 e2
    g |> (Equal e1 e2) = assertEquality g e1 e2
    g |> (NotEqual e1 e2) = assertEquality g e1 e2
    g |> (Not e) =
        let t = g |> e
         in assert (t == Valid BoolP) ("Could not unify Bool and " ++ show t) t
    g |> (AndStrict e1 e2) = assertExpr g RealP e1 e2
    g |> (AndLazy e1 e2) = assertExpr g RealP e1 e2
    g |> (OrStrict e1 e2) = assertExpr g RealP e1 e2
    g |> (OrLazy e1 e2) = assertExpr g RealP e1 e2
    g |> (Implies e1 e2) = assertExpr g RealP e1 e2
    g |> (Xor e1 e2) = assertExpr g RealP e1 e2
    g |> (ShiftLeft e1 e2) = assertExpr g IntP e1 e2
    g |> (ShiftRight e1 e2) = assertExpr g IntP e1 e2
    g |> (ShiftRightSameSign e1 e2) = assertExpr g IntP e1 e2
    g |> (Set (e:es)) =
        let t = g |> e
         in assert
                (all (== t) ((g |>) <$> es))
                "All elements of a set must have the same type"
                (case t of
                     Valid t' -> Valid $ ESet t'
                     x -> x)
    g |> (List (e:es)) =
        let t = g |> e
         in assert
                (all (== t) ((g |>) <$> es))
                "All elements of a set must have the same type"
                (case t of
                     Valid t' -> Valid $ EList t'
                     x -> x)
    g |> (Tuple es) =
        if all validType tjs
            then Valid (ETuple ts)
            else head (filter (not . validType) tjs)
      where
        tjs = (g |>) <$> es
        ts = unbox tjs
    g |> (Call c) = g |> c -- TODO: Judge the type, check that it is a subtype of what is expected
    -- g |> (PureCallExpr (PureCall (Ident p) es)) = case get p g of
    --                                         Valid (EFunction pty ti to) ->
    --                                             assert 
    --                                                 (
    --                                                     (g |- (pty <: Pure)) == Pass &&
    --                                                     all validType ((g |>) <$> (\(x,y) -> (x <: y)) <$> zip <$> es ts)
    --                                                     -- all (\(e,t) -> validType (g |> (e <: t))) (zip es ts)
    --                                                 )
    --                                                 (i ++ " cannot be used as a pure function")
    --                                             to
    --                                         x -> x
    -- g |> (ImpureCallExpr (ImpureCall (Ident i) e)) =
    --     case get i g of
    --         Valid (EFunction pty ti to) ->
    --             assert
    --                 ((g |- (pty <: Impure)) == Pass && (g |- (ti <: e)) == Pass
    --                                                     -- all validType ((g |>) <$> (\(x,y) -> (x <: y)) <$> zip <$> es ts)
    --                                                     -- all (\(e,t) -> validType (g |> (e <: t))) (zip es ts)
    --                  )
    --                 (i ++ " cannot be used as an impure function")
    --                 to
    --         x -> x
    -- g |> (ImpureCallExpr (ImpureCall (Ident i) _)) = case get i g of
    --                                                     Valid t -> EFunction Impure t
    --                                                     x -> x

-- instance Typable Call where
--     g |> (PartialApplication)
assertExpr ::
       Typable a
    => Typable b =>
           TypeEnvironment -> EmperorType -> a -> b -> TypeJudgementResult
assertExpr g t e1 e2 =
    let t1 = g |> e1
     in case t1 of
            Valid t1' ->
                let t2 = g |> e2
                 in case t2 of
                        Valid t2' ->
                            assert
                                ((g |- (t1' <: t)) == Pass &&
                                 (g |- (t2' <: t)) == Pass)
                                ("Could not unify " ++
                                 show t1' ++
                                 ", " ++ show t2' ++ " <: " ++ show t)
                                (Valid t)
                        x -> x
            x -> x

assertEquality ::
       Typable a
    => Typable b =>
           TypeEnvironment -> a -> b -> TypeJudgementResult
assertEquality g e1 e2 =
    let t1 = g |> e1
     in case t1 of
            Valid t1' ->
                let t2 = g |> e2
                 in case t2 of
                        Valid t2' ->
                            assert
                                (all (\j -> (g |- j) == Pass)
                                     [ t1' <: (unsafeGet eqable g)
                                     , t2' <: t1'
                                     , t2' <: t1'
                                     ])
                                ("Could not unify " ++
                                 show t1' ++ " = " ++ show t2')
                                (Valid t1')
                        x -> x
            x -> x

instance Typable Value where
    _ |> (Integer _) = Valid IntP
    _ |> (Real _) = Valid RealP
    _ |> (Char _) = Valid CharP
    _ |> (Bool _) = Valid BoolP
    -- _ |> (String _) = Valid $ EList CharP
    g |> (IdentV i) = get i g

assert :: Bool -> String -> TypeJudgementResult -> TypeJudgementResult
assert False s _ = Invalid s
assert _ _ j = j

-- | Unbox a list of VALID type judgements. Note that this operation is unsafe.
unbox :: [TypeJudgementResult] -> [EmperorType]
unbox (Invalid _:_) =
    error $
    "Failed assertion that all of " ++ show tjs ++ " are valid type judgements"
unbox (Valid t:tjs') = t : unbox tjs'
