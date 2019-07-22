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
module Subtypes (typeCheck, Type) where

import AST
import Data.Map
import Types (EmperorType(..), EmperorPrimitiveType(..))

newtype TypeEnvironment = TypeEnvironment (Map String EmperorType)

-- data TypePair = TypePair a b
-- type TypePair a b = (a,b)
data TypeComparison a b = SubType a b

(<:) :: Type a => Type b => a -> b -> TypeComparison a b
infixl 1 <:
a <: b = SubType a b

-- a <:> b = SuperType a b
-- infixl 1 <:>

class Eq a => Type a where
    infixl 0 |-
    (|-) :: Type b => TypeEnvironment -> TypeComparison a b -> Bool

instance Type EmperorPrimitiveType where
    _ |- (SubType IntP RealP) = True
    _ |- (SubType a b) = a == b
    
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
