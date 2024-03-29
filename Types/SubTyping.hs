{-|
Module      : SubTyping
Description : Rules for subtypes
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

This module defines the subtyping rules for Emperor
-}
module Types.SubTyping
    ( (|-)
    , (<:)
    , SubTypable
    , TypeComparison
    ) where

import Data.List ((\\))
import Data.Map ((!), keys)
import Types.Environment (TypeEnvironment(..))
import Types.Results (EmperorType(..), Purity(..), TypeCheckResult(..))

-- | Represents a comparison between two types
data TypeComparison a b =
    SubType a b

instance (Show a, Show b) => Show (TypeComparison a b) where
    show (SubType a b) = show a ++ " <: " ++ show b

infixl 3 <:

-- | Sets up a type-comparison
(<:) ::
       SubTypable a
    => SubTypable b =>
           a -> b -> TypeComparison a b
a <: b = SubType a b

-- | Type class to describe objects upon which the subtype relation is valid
class SubTypable a where
    infixl 2 |-
    -- | Judge the validity of a type comparison
    (|-) :: TypeEnvironment -> TypeComparison a a -> TypeCheckResult

instance SubTypable EmperorType where
    _ |- (SubType a b)
        | a == b = Pass
    _ |- (SubType _ Any) = Pass
    _ |- (SubType Unit _) = Pass
    _ |- (SubType IntP RealP) = Pass
    e |- (SubType (EList a) (EList b)) = e |- a <: b
    e |- (SubType (ETuple as) (ETuple bs)) =
        if all (== Pass) typeResults
            then Pass
            else head $ dropWhile (== Pass) typeResults
      where
        typeResults = (e |-) <$> comparisons
        comparisons = (<:) <$> as <*> bs
    e |- (SubType (ERecord as) (ERecord bs))
        | keys bs `subset` keys as && all (\k -> (e |- (as ! k <: bs ! k)) == Pass) (keys bs) = Pass
        | otherwise = typeCheckFail (SubType (as ! b) (bs ! b))
      where
        b = head $ filter (\k -> (e |- (as ! k <: bs ! k)) /= Pass) $ keys as
    e |- (SubType (EFunction p i o) (EFunction p' i' o')) =
        case e |- p <: p' of
            Pass ->
                case e |- i' <: i of
                    Pass -> e |- o <: o'
                    x -> x
            x -> x
    _ |- c = typeCheckFail c

instance SubTypable Purity where
    _ |- (SubType Pure Impure) = Fail "Violated type constraint: Pure <: Impure"
    _ |- _ = Pass

subset :: Eq a => [a] -> [a] -> Bool
subset as bs = null $ bs \\ as

typeCheckFail ::
       Show a
    => Show b =>
           TypeComparison a b -> TypeCheckResult
typeCheckFail c = Fail $ "Type-checking error: " ++ show c ++ " does not hold!"
