{-|
Module      : Judger
Description : Type judge for emperor
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

This module defines typing judgements for the AST. Types are found and checked 
for compatibility.
-}
module Types.Judger
    ( (|>)
    , judge
    , Typable
    ) where

import AST (Expr(..), Ident(..), PartialCall(..), Value(..), getPurity)
import Types.Checker ((<:), (|-))
import Types.Environment (TypeEnvironment(..), get, newTypeEnvironment, unsafeGet)
import Types.PreludeTypes (PreludeType, eqable)
import Types.Results (EmperorType(..), Purity(..), TypeCheckResult(..), TypeJudgementResult(..), isValid, isValidAnd)

-- | Class describing constructs which may be assigned a type.
class Typable a where
    judge :: a -> TypeJudgementResult
    -- ^ Obtain the type of a given construct from a fresh environment.
    judge = (newTypeEnvironment |>)
    infixl 2 |>
    -- | Judge the type of a given construct under a particular typing environment.
    (|>) :: TypeEnvironment -> a -> TypeJudgementResult

instance Typable Expr where
    g |> (Value v) = g |> v
    g |> (Neg e) =
        case g |> e of
            Valid t -> assert ((g |- t <: RealP) == Pass) ("Could not unify Bool and " ++ show t) (Valid t)
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
        case g |> e of
            Valid BoolP -> Valid BoolP
            Valid t -> Invalid $ "Could not unify bool and " ++ show t
            x -> x
    g |> (AndStrict e1 e2) = assertExpr g RealP e1 e2
    g |> (AndLazy e1 e2) = assertExpr g RealP e1 e2
    g |> (OrStrict e1 e2) = assertExpr g RealP e1 e2
    g |> (OrLazy e1 e2) = assertExpr g RealP e1 e2
    g |> (Implies e1 e2) = assertExpr g RealP e1 e2
    g |> (Xor e1 e2) = assertExpr g RealP e1 e2
    g |> (ShiftLeft e1 e2) = assertExpr g IntP e1 e2
    g |> (ShiftRight e1 e2) = assertExpr g IntP e1 e2
    g |> (ShiftRightSameSign e1 e2) = assertExpr g IntP e1 e2
    _ |> (Set []) = Valid $ ESet Any
    g |> (Set (e:es)) =
        case g |> e of
            Valid t ->
                assert
                    (all (isValidAnd t) ((g |>) <$> es))
                    "All elements of a set must have the same type"
                    (Valid $ EList t)
            x -> x
    _ |> (List []) = Valid $ EList Any
    g |> (List (e:es)) =
        case g |> e of
            Valid t ->
                assert
                    (all (isValidAnd t) ((g |>) <$> es))
                    "All elements of a list must have the same type"
                    (Valid $ EList t)
            x -> x
    g |> (Tuple es) =
        if all isValid tjs
            then Valid (ETuple ts)
            else head (filter (not . isValid) tjs)
      where
        tjs = (g |>) <$> es
        ts = unbox tjs

assertExpr ::
       Typable a
    => Typable b =>
           TypeEnvironment -> EmperorType -> a -> b -> TypeJudgementResult
assertExpr g t e1 e2 =
    case g |> e1 of
        Valid t1 ->
            case g |> e2 of
                Valid t2 ->
                    assert
                        ((g |- t1 <: t) == Pass && (g |- t2 <: t) == Pass)
                        ("Could not unify " ++ show t1 ++ ", " ++ show t2 ++ " <: " ++ show t)
                        (Valid t)
                x -> x
        x -> x

assertEquality ::
       Typable a
    => Typable b =>
           TypeEnvironment -> a -> b -> TypeJudgementResult
assertEquality g e1 e2 =
    case g |> e1 of
        Valid t1 ->
            case g |> e2 of
                Valid t2 ->
                    assert
                        (all (\j -> (g |- j) == Pass) [t1 <: unsafeGet eqable g, t2 <: t1, t2 <: t1])
                        ("Could not unify " ++ show t1 ++ " = " ++ show t2)
                        (Valid t1)
                x -> x
        x -> x

instance Typable Value where
    _ |> (Integer _) = Valid IntP
    _ |> (Real _) = Valid RealP
    _ |> (Char _) = Valid CharP
    _ |> (Bool _) = Valid BoolP
    _ |> (IDC) = Valid Unit
    -- _ |> (String _) = Valid $ EList CharP
    -- g |> (IdentV i) = get i g
    g |> (Call c) = g |> c -- TODO: Judge the type, check that it is a subtype of what is expected

instance Typable PartialCall where
    g |> (PartialApplication p e) =
        case g |> e of
            Valid u ->
                case g |> p of
                    Valid (EFunction _ t1 t2) ->
                        assert
                            (isValid $ g |- u <: t1)
                            ("Type assertion does not hold " ++ show u ++ " <: " ++ show t1)
                            (Valid t2)
                    Valid x -> Invalid $ "Could not apply type " ++ show u ++ " to " ++ show x
                    x -> x
            x -> x
    g |> (CallIdentifier pty (Ident i)) =
        case get i g of
            Valid (EFunction pty' t1 t2) ->
                assert
                    (isValid $ g |- pty <: pty')
                    ("Purity mismatch in call to " ++ show i)
                    (Valid $ EFunction pty' t1 t2)
            x -> x

assert :: Bool -> String -> TypeJudgementResult -> TypeJudgementResult
assert False s _ = Invalid s
assert _ _ j = j

-- | Unbox a list of VALID type judgements. Note that this operation is unsafe.
unbox :: [TypeJudgementResult] -> [EmperorType]
unbox [] = []
unbox (Invalid _:_) = error "Failed assertion that all of a list of type judgements are valid type judgements"
unbox (Valid t:tjs') = t : unbox tjs'
