{-|
Module      : Imports
Description : Handle import calls
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

This module defines machinery to handle imports and obtain the type-environments
they export.
-}
module Types.Imports
    ( importFile
    , ImportBehaviour
    ) where

import Types.Environment (TypeEnvironment, newTypeEnvironment)

-- | Import the type environment created by a single file
importFile :: FilePath -> IO TypeEnvironment
importFile _ = newTypeEnvironment

-- TODO: add option to import from header file or recurse
-- | Set import behaviour
data ImportBehaviour
    = Static -- ^ Read from a pre-generated header (e.g. for Make-like runs)
    | Recurse -- ^ Go through each dependency in this compilation run
    deriving (Eq, Show)
