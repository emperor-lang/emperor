{-|
Module      : TypedAST
Description : Typed abstract syntax tree
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

Data definitions for the typed AST
-}
module Types.TypedAST (TypedAST(..)) where

import Types.Environment (TypeEnvironment)

data TypedAST = None
    deriving Show
