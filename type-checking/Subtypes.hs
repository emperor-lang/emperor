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
module Subtypes (TypeCheckResult(..), (|-), (<:), newTypeEnvironment) where

import AST
import Data.List ((\\))
import Data.Map hiding ((\\), filter, null)
import Types (EmperorType(..))

type TypeEnvironment = Map String EmperorType

newTypeEnvironment :: TypeEnvironment
newTypeEnvironment = empty

-- data TypePair = TypePair a b
-- type TypePair a b = (a,b)
data TypeComparison = SubType EmperorType EmperorType

(<:) :: EmperorType -> EmperorType -> TypeComparison
infixl 1 <:
a <: b = SubType a b

data TypeCheckResult = Fine
                     | Bad String
    deriving (Eq, Ord, Show)

infixl 0 |-
(|-) :: TypeEnvironment -> TypeComparison -> TypeCheckResult
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

-- class Eq a => Type a where
--     infixl 0 |-
--     -- (|-) :: TypeEnvironment -> TypeComparison a b -> TypeCheckResult
--     -- (|>) :: 

-- instance Type EmperorPrimitiveType where
--     -- _ |- (SubType IntP RealP) = Fine
--     _ |- (SubType a b)
--         | a == b    = Fine 
--         | otherwise = Bad $ "Type error: " ++ show a ++ " <: " ++ show b ++ " does not hold!"
    
-- instance Type EmperorType where
--     e |- (SubType a b)
--         | a == b = Fine

-- (<:>) :: Type a => Type b => a -> b -> (a, b)
-- infixl 1 <:>
-- a <:> b = (b, a)

-- type TypePair = (EmperorType, EmperorType)
-- data TypePair = TypePair EmperorType EmperorType

-- (<:) :: EmperorType -> EmperorType -> TypePair
-- infixl 1 <:
-- a <: b = TypePair a b

-- -- (:>) :: Type a => Type b => a -> b -> TypePair a b
-- -- infixl 1 :>
-- -- b :> a = TypePair a b

-- -- | Type class to represent emperor types
-- class Type a where

-- -- | Integers are a sub-type of real numbers, no other primitive is a subtype of
-- -- any other
-- instance Type EmperorPrimitiveType where
--     _ |- (IntP, RealP) = True
--     _ |- (x, y) = x == y

-- instance Type EmperorType where
--     e |- ((EPrimitive a), (EPrimitive b))   = e |- (a <: b)
--     e |- ((EList a), (EList b))             = e |- (a <: b)
--     e |- ((ETuple as), (ETuple bs))         = all $ (e |-) <$> zip as bs
--     e |- ((ERecord as), (ERecord bs))       = (keys as) `subset` (keys bs) 
--                                             && and [ e |- (as ! k <: bs ! k) | k <- keys as]

-- -- | One emperor list is a sub-type of another if the type of their elements are
-- -- sub-types
-- instance Type (EmperorList a) where
--     e |- ((EmperorList a), (EmperorList b)) = e |- a <: b

-- -- | Emperor-tuples are lists of their corresponding types
-- data EmperorTuple = Type a => EmperorTuple [a]

-- -- | One tuple is a subtype of another if all elements are sub-types
-- instance Type EmperorTuple where
--     (EmperorTuple as) <: (EmperorTuple bs) = and $ (<:) <$> as <*> bs
