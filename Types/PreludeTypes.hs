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
module Types.PreludeTypes (eqable, PreludeType) where

-- | Type of items in the prelude
type PreludeType = String

-- | Class of types upon which equality may be called
eqable :: PreludeType
eqable = "Eq"
