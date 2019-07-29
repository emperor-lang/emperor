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
module Types.Environment (get, newTypeEnvironment, TypeEnvironment) where

import Prelude hiding (lookup)
import Data.Map (Map, empty, lookup)
import Types.Results (EmperorType, TypeJudgementResult(..))

-- | An environment which maps names to types
type TypeEnvironment = Map String EmperorType

-- | Creates a fresh type-environment
newTypeEnvironment :: TypeEnvironment
newTypeEnvironment = empty

-- | Get a value from a type environment
get :: String -> TypeEnvironment -> TypeJudgementResult
get s g = case lookup s g of
            Just t  -> Valid t
            Nothing -> Invalid $ "Type unknown in current environment" ++ s
