{-|
Module      : Types
Description : Typing data constructors
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

This defines data to represent types in Emperor.
-}
module Types (EmperorType(..), EmperorPrimitiveType(..)) where

-- | Data to represent Emperor composite types
data EmperorType = EPrimitive EmperorPrimitiveType
                 | EList EmperorType
                 | ETuple [EmperorType]
                 | ERecord [(String,EmperorType)]
    deriving Eq

-- | A primitive Emperor type
data EmperorPrimitiveType = IntP
                          | CharP
                          | BoolP
                          | RealP
    deriving Eq

-- -- | A single Emperor type
-- newtype EmperorTypeInstance = EmperorType String

-- -- | List type for emperor objects
-- newtype EmperorList a = EmperorList a

-- -- | Tuple type for emperor objects
-- newtype EmperorTuple 