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

import AST (AST(..), BodyLine(..), Ident(..), Value(..))
import Data.List (unwords)
import Formatter (formatFresh)
import Types.Environment (TypeEnvironment)
import Types.Resolver ((|>), judge)
import Types.Results (EmperorType, TypeJudgementResult(..))

-- | Find any problems with the typing of results and obtain types if no 
-- problems are found.
resolveTypes :: AST -> Either String [Function]
resolveTypes _ = Left "Type checking has not been implemented yet."

-- | Describes the functions and constants to be translated.
data Function
    = Function Ident EmperorType [(Ident, EmperorType)] [BodyLine]
    | Constant Ident EmperorType Value

instance Show Function
    -- TODO: Complete the instance of Show for functions
                                                         where
    show (Function i t ps _) = show i ++ " :: " ++ show t ++ "\n" ++ show i ++ " " ++ showParameters ps ++ " = " ++ "??"
      where
        showParameters :: [(Ident, EmperorType)] -> String
        showParameters ps' = unwords $ showParameter <$> ps'
        showParameter :: (Ident, EmperorType) -> String
        showParameter (i', _) = formatFresh i'
    show (Constant i t v) = unwords [formatFresh i, show t, formatFresh v]
