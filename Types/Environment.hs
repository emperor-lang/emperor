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
module Types.Environment
    ( get
    , newTypeEnvironment
    , TypeEnvironment(..)
    , unsafeGet
    ) where

import Data.Map (Map, empty, lookup)
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)
import Types.Results (EmperorType, TypeJudgementResult(..))

-- | An environment which maps names to types
data TypeEnvironment = TypeEnvironment (Map String EmperorType)
    deriving (Show)

-- | Creates a fresh type-environment
newTypeEnvironment :: TypeEnvironment
newTypeEnvironment = TypeEnvironment empty

-- | Get a value from a type environment
get :: String -> TypeEnvironment -> TypeJudgementResult
get s (TypeEnvironment g) =
    case lookup s g of
        Just t -> Valid t
        Nothing -> Invalid $ "Type unknown in current environment" ++ s
        
-- | Get a value from a type environment under the assertion that it already
-- exists. This should only be used for types guaranteed to be in the prelude.
unsafeGet :: String -> TypeEnvironment -> EmperorType
unsafeGet s (TypeEnvironment g) = fromMaybe (error $ "Could not find type " ++ s) (lookup s g)
