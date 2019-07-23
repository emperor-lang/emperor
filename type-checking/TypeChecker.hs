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
module TypeChecker (typeCheck, TypeCheckResult(..)) where

import AST (AST)

data TypeCheckResult = Fine
                     | Bad String

typeCheck :: AST -> TypeCheckResult
typeCheck _ = Fine -- Bad "Type-checking has not been implemented yet"
