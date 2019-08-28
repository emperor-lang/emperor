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

import Parser.AST (Call(..), Expr(..), Ident(..), Value(..))
import Types.Environment (TypeEnvironment(..), (=>>), newTypeEnvironment, unsafeGet)
import Types.PreludeTypes (eqable)
import Types.Results
    ( EmperorType(..)
    , Purity(..)
    , TypeCheckResult(..)
    , TypeJudgementResult(..)
    , getTypeList
    , isValid
    , isValidAnd
    , unpackTypes
    )
import Types.SubTyping ((<:), (|-))

-- | Class describing constructs which may be assigned a type.
class Typable a where
    judge :: a -> TypeJudgementResult
    -- ^ Obtain the type of a given construct from a fresh environment.
    judge = (newTypeEnvironment |>)
    infixl 2 |>
    -- | Judge the type of a given construct under a particular typing environment.
    (|>) :: TypeEnvironment -> a -> TypeJudgementResult

instance Typable Expr where
    g |> (Value v _) = g |> v
    g |> (Neg e _) =
        case g |> e of
            Valid t -> assert ((g |- t <: RealP) == Pass) ("Could not unify Bool and " ++ show t) (Valid t)
            x -> x
    g |> (Add e1 e2 _) = arithExpr g e1 e2
    g |> (Subtract e1 e2 _) = arithExpr g e1 e2
    g |> (Multiply e1 e2 _) = arithExpr g e1 e2
    g |> (Divide e1 e2 _) = arithExpr g e1 e2
    g |> (Modulo e1 e2 _) = arithExpr g e1 e2
    g |> (Less e1 e2 _) = arithExpr g e1 e2
    g |> (LessOrEqual e1 e2 _) = arithExpr g e1 e2
    g |> (Greater e1 e2 _) = arithExpr g e1 e2
    g |> (GreaterOrEqual e1 e2 _) = arithExpr g e1 e2
    g |> (Equal e1 e2 _) = assertEquality g e1 e2
    g |> (NotEqual e1 e2 _) = assertEquality g e1 e2
    g |> (Not e _) =
        case g |> e of
            Valid BoolP -> Valid BoolP
            Valid t -> Invalid $ "Could not unify bool and " ++ show t
            x -> x
    g |> (AndStrict e1 e2 _) = assertExpr g BoolP e1 e2
    g |> (AndLazy e1 e2 _) = assertExpr g BoolP e1 e2
    g |> (OrStrict e1 e2 _) = assertExpr g BoolP e1 e2
    g |> (OrLazy e1 e2 _) = assertExpr g BoolP e1 e2
    g |> (Implies e1 e2 _) = assertExpr g BoolP e1 e2
    g |> (Xor e1 e2 _) = assertExpr g BoolP e1 e2
    g |> (ShiftLeft e1 e2 _) = assertExpr g IntP e1 e2
    g |> (ShiftRight e1 e2 _) = assertExpr g IntP e1 e2
    g |> (ShiftRightSameSign e1 e2 _) = assertExpr g IntP e1 e2
    _ |> (Set [] _) = Valid $ ESet Any
    g |> (Set (e:es) _) =
        case g |> e of
            Valid t ->
                assert
                    (all (isValidAnd t) ((g |>) <$> es))
                    "All elements of a set must have the same type"
                    (Valid $ EList t)
            x -> x
    _ |> (List [] _) = Valid $ EList Any
    g |> (List (e:es) _) =
        case g |> e of
            Valid t ->
                assert
                    (all (isValidAnd t) ((g |>) <$> es))
                    "All elements of a list must have the same type"
                    (Valid $ EList t)
            x -> x
    g |> (Tuple es _) =
        if all isValid tjs
            then Valid (ETuple ts)
            else head (filter (not . isValid) tjs)
      where
        tjs = (g |>) <$> es
        ts = unbox tjs

arithExpr ::
       Typable a
    => Typable b =>
           TypeEnvironment -> a -> b -> TypeJudgementResult
arithExpr g e1 e2 =
    case g |> e1 of
        Valid IntP ->
            case g |> e2 of
                Valid IntP -> Valid IntP
                Valid t -> Invalid $ "Cannot unify int and " ++ show t
                x -> x
        Valid RealP ->
            case g |> e2 of
                Valid RealP -> Valid RealP
                Valid t -> Invalid $ "Cannot unify real and " ++ show t
                x -> x
        Valid t -> Invalid $ "Cannot use " ++ show t ++ " in arithmetic expressions."
        Invalid m -> Invalid m

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
    _ |> (Integer _ _) = Valid IntP
    _ |> (Real _ _) = Valid RealP
    _ |> (Char _ _) = Valid CharP
    _ |> (Bool _ _) = Valid BoolP
    _ |> (IDC _) = Valid Unit
    _ |> (StringV _ _) = Valid $ EList CharP
    g |> (IdentV (Ident i _) _) = g =>> i
    g |> (CallV c _) = g |> c

instance Typable Call where
    g |> (Call p (Ident i _) es _) =
        if unsafeGet "@" g == Unit && p == Impure
            then Invalid "Impure call in pure environment"
            else case g =>> i of
                Valid t ->
                    case t of
                        EFunction p' t1 t2 ->
                            case g |- p <: p' of
                                Pass ->
                                    let types = getTypeList (EFunction p' t1 t2)
                                    in let expectedInputTypes = init types
                                        in let expectedOutputType = last types
                                            in if length expectedInputTypes == length es
                                                    then let inputTypeJudgements = (g |>) <$> es
                                                        in if all isValid inputTypeJudgements
                                                                then case unpackTypes inputTypeJudgements of
                                                                        Right inputTypes ->
                                                                            if all isValid $
                                                                                (g |-) <$>
                                                                                zipWith (<:) inputTypes expectedInputTypes
                                                                                then Valid expectedOutputType
                                                                                else Invalid
                                                                                        "Some input type is not a sub-type of the expected"
                                                                        Left m -> Invalid m
                                                                else head $ filter (not . isValid) inputTypeJudgements
                                                    else Invalid $
                                                        "Expected " ++
                                                        (show . length) expectedInputTypes ++
                                                        " inputs to function, got " ++ (show . length) es
                                Fail m -> Invalid m
                        x -> Invalid $ "Expected function type, got " ++ show x ++ " instead."
                x -> x

assert :: Bool -> String -> TypeJudgementResult -> TypeJudgementResult
assert False s _ = Invalid s
assert _ _ j = j

-- | Unbox a list of VALID type judgements. Note that this operation is unsafe.
unbox :: [TypeJudgementResult] -> [EmperorType]
unbox [] = []
unbox (Invalid _:_) = error "Failed assertion that all of a list of type judgements are valid type judgements"
unbox (Valid t:tjs') = t : unbox tjs'
