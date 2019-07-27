{-|
Module      : TypeEnvironment
Description : Typing results for emperor
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

This module defines the type environments of Emperor programs.
-}
module Types.Environment (newTypeEnvironment, TypeEnvironment) where

import Data.Map (Map, empty)
import Types.Results (EmperorType)

-- | An environment which maps names to types
type TypeEnvironment = Map String EmperorType

-- | Creates a fresh type-environment
newTypeEnvironment :: TypeEnvironment
newTypeEnvironment = empty