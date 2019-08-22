{-|
Module      : Optimise
Description : Optimiser for Emperor programs
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

Performs optimisations on Emperor programs.
-}
module Optimiser.Optimise (optimiseAST) where

import Args (Args)
import Parser.AST (AST)

-- | Perform semantic optimisations
optimiseAST :: Args -> AST -> AST
optimiseAST _ = id
