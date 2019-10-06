{-|
Module      : RResolver
Description : Type resolver for emperor
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

This module defines typing judgements for the AST. Types are found and checked
for compatibility.
-}
module Types.PreludeTypes
    ( eqable
    , PreludeType
    ) where

import           Parser.AST          (Ident (..))
import           Parser.EmperorLexer (AlexPosn (..))

-- | Type of items in the prelude
type PreludeType = Ident

-- | Class of types upon which equality may be called
eqable :: PreludeType
eqable = Ident "Eq" (Just "base") (AlexPn 0 0 0)
