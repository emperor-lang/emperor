module Checker ((|-), (<:)) where

import Data.List ((\\))
import Data.Map ((!), empty, keys, Map)
import Results (EmperorType(..), TypeCheckResult(..))
import TypeEnvironment (TypeEnvironment)

-- | Represents a comparison between two types
data TypeComparison = SubType EmperorType EmperorType

instance Show TypeComparison where
    show (SubType a b) = show a ++ " <: " ++ show b

-- | Sets up a type-comparison
(<:) :: EmperorType -> EmperorType -> TypeComparison
infixl 1 <:
a <: b = SubType a b

-- | Execute a type comparison
infixl 0 |-
(|-) :: TypeEnvironment -> TypeComparison -> TypeCheckResult
_ |- (SubType _ Any) = Pass
_ |- (SubType Unit _) = Pass
_ |- (SubType a b)
    | a == b = Pass
_ |- (SubType IntP RealP) = Pass
e |- (SubType (EList a) (EList b)) = e |- (a <: b)
e |- (SubType (ETuple as) (ETuple bs)) = if all (== Pass) typeResults
        then Pass
        else head $ dropWhile (== Pass) typeResults
    where
        typeResults = (e |-) <$> comparisons
        comparisons = (<:) <$> as <*> bs
e |- (SubType (ERecord s as) (ERecord s' bs))
    | s == s' 
        && keys bs `subset` keys as
        && all (\k -> (e |- ((as ! k) <: (bs ! k))) == Pass) (keys bs)
        = Pass
    | otherwise = typeCheckFail (SubType (as ! b) (bs ! b))
        where
            b = head $ filter (\k -> (e |- ((as ! k) <: (bs ! k))) /= Pass) $ keys as
e |- (SubType (EFunction i o) (EFunction i' o')) = case e |- (i' <: i) of
                                                    Pass -> e |- (o <: o')
                                                    x -> x 
_ |- c = typeCheckFail c

subset :: Eq a => [a] -> [a] -> Bool
subset as bs = null $ bs \\ as

typeCheckFail :: TypeComparison -> TypeCheckResult
typeCheckFail c = Fail $ "Type-checking error: " ++ show c ++ " does not hold!"
