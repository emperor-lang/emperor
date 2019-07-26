module TypeEnvironment (newTypeEnvironment, TypeEnvironment) where

import Data.Map (Map, empty)
import Results (EmperorType)

-- | An environment which maps names to types
type TypeEnvironment = Map String EmperorType

-- | Creates a fresh type-environment
newTypeEnvironment :: TypeEnvironment
newTypeEnvironment = empty