{-|
Module      : Results
Description : Typing results for emperor
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

This module defines valid type structures and the results which may be returned
by the argument checker or resolver.
-}
module Types.Results (
    EmperorType(..), TypeCheckResult(..), TypeJudgementResult(..), Purity(..)
) where

import Data.Map (Map)

-- | The result of a typing judgement. This is either an error indicating a
-- problem or a type
data TypeJudgementResult = Valid EmperorType -- ^ Valid type
                         | Invalid String -- ^ Indicates an incorrect type and 
                                          -- gives an explanation
    deriving (Eq, Show)

-- | The result of a type-check; this indicates a valid or invalid relation.
-- Where this is invalid, a message is given.
data TypeCheckResult = Pass -- ^ Indicates a correct type statement
                     | Fail String -- ^ Indicates an invalid type and provides a
                                   -- reason
    deriving (Eq)

-- instance Functor TypeCheckResult where
--     fmap f Pass = Pass
--     fmap f (Fail s) = Fail $ f s

-- | Marker for whether a function is pure or impure
data Purity = Pure
            | Impure
    deriving (Eq, Show)

-- | Data to represent all Emperor types
data EmperorType = IntP -- ^ Integer primitive
                 | CharP -- ^ Character primitive
                 | BoolP -- ^ Boolean primitive
                 | RealP -- ^ Real number primitive
                 | ESet EmperorType -- ^ Set composite
                 | EList EmperorType -- ^ List composite
                 | ETuple [EmperorType] -- ^ Tuple composite
                 | ERecord String (Map String EmperorType) -- ^ Record composite
                 | EFunction Purity EmperorType EmperorType -- ^ Function composite
                 | Any -- ^ Universal super-type
                 | Unit -- ^ Universal sub-type
    deriving (Eq, Show)