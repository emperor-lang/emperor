{-|
Module      : Types
Description : Wrapper for the emperor typing machinery
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

This module enables the type checking and judgement modules to be called more 
easily.
-}
module Types.Types
    ( resolveTypes
    , TypeJudgementResult(..)
    , Function(..)
    ) where

import AST (AST)
import Types.Environment (TypeEnvironment)
import Types.Resolver ((|>), judge)
import Types.Results (EmperorType, TypeJudgementResult(..))

-- | Find any problems with the typing of results and obtain types if no 
-- problems are found.
resolveTypes :: AST -> Either String [Function]
resolveTypes _ = Left "Type checking has not been implemented yet."

-- | Describes the functions and constants to be translated.
data Function
    = Const -- Ident Value
    | Function -- Identifier EmperorType Body
    deriving (Show)

-- data Body
