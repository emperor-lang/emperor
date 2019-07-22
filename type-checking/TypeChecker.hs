{-|
Module      : TypeChecker
Description : Type-checker for Emperor
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

This module resolves types for Emperor programs given as an AST
-}
module TypeChecker (typeCheck) where

import AST 

typeCheck :: AST -> Either String String
typeCheck _ = Left "Type-checking has not been implemented yet"
