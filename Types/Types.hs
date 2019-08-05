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
module Types.Types (resolveTypes) where

import AST (AST(..))
import Types.Resolver (resolve)
import Types.Environment (TypeEnvironment)
import Types.TypedAST (TypedAST)

-- | Find any problems with the typing of results and obtain types if no 
-- problems are found.
resolveTypes :: AST -> Either String (TypedAST, TypeEnvironment)
resolveTypes _ = Left "Type checking has not been implemented yet."
