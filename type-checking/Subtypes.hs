{-|
Module      : Subtypes
Description : Typing rules for emperor
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

This modules defines sub-typing rules for Emperor types
-}
module Subtypes (TypeCheckResult(..), (|-), (<:), newTypeEnvironment, TypeEnvironment, TypeComparison) where

import AST
import Data.List ((\\))
import Data.Map hiding ((\\), filter, null)
import Types (EmperorType(..))

-- | A mapping from names to their respective types
type TypeEnvironment = Map String EmperorType

-- | Creates a fresh type-environment
newTypeEnvironment :: TypeEnvironment
newTypeEnvironment = empty

-- | Represents a comparison between two types
data TypeComparison = SubType EmperorType EmperorType

-- | Sets up a type-comparison
(<:) :: EmperorType -> EmperorType -> TypeComparison
infixl 1 <:
a <: b = SubType a b

-- | Indicates whether a type-check has been successful
data TypeCheckResult = Fine       -- ^ Indicates check has worked
                     | Bad String -- ^ Indicates check has failed and gives a reason
    deriving (Eq, Ord, Show)

-- | Type judgement
(|-) :: TypeEnvironment -> TypeComparison -> TypeCheckResult
infixl 0 |-
_ |- (SubType _ Any) = Fine
_ |- (SubType Unit _) = Fine
_ |- (SubType a b)
    | a == b = Fine
_ |- (SubType IntP RealP) = Fine
e |- (SubType (EList a) (EList b)) = e |- (a <: b)
e |- (SubType (ETuple as) (ETuple bs)) = if all (== Fine) typeResults
        then Fine
        else head $ dropWhile (== Fine) typeResults
    where
        typeResults = (e |-) <$> comparisons
        comparisons = (<:) <$> as <*> bs
e |- (SubType (ERecord as) (ERecord bs))
    | keys bs `subset` keys as && all (\k -> (e |- ((as ! k) <: (bs ! k))) == Fine) (keys bs) = Fine
    | otherwise = typeCheckFail (as ! b) (bs ! b)
        where
            b = head $ filter (\k -> (e |- ((as ! k) <: (bs ! k))) /= Fine) $ keys as
e |- (SubType (EFunction i o) (EFunction i' o')) = case e |- (i' <: i) of
                                                    Fine -> e |- (o <: o')
                                                    x -> x 
_ |- (SubType t1 t2)  = typeCheckFail t1 t2

subset :: Eq a => [a] -> [a] -> Bool
subset as bs = null $ bs \\ as

typeCheckFail :: EmperorType -> EmperorType -> TypeCheckResult
typeCheckFail a b = Bad $ "Type-checking error: " ++ show a ++ " <: " ++ show b ++ " does not hold!"
