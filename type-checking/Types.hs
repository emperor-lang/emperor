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
module Types (EmperorType(..)) where

import Data.Map

-- | Data to represent all Emperor types
data EmperorType = IntP
                 | CharP
                 | BoolP
                 | RealP
                 | EList EmperorType
                 | ETuple [EmperorType]
                 | ERecord (Map String EmperorType)
                 | EFunction EmperorType EmperorType
                 | Any
                 | Unit
    deriving (Eq, Show)