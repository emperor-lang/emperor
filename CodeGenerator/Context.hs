{-|
Module      : Generate
Description : Code generator
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

This module handles the generation of C code from an AST. Output is given as a
pair of strings, the first containing C to execute, and the second containing
its C header.
-}
module CodeGenerator.Context
    ( GenerationContext
    , indent
    , isEntryPoint
    , makeContext
    , sourceFile
    ) where

import Args (Args, entryPoint, input)

data GenerationContext =
    GenerationContext
        { isEntryPoint :: Bool
        , sourceFile :: FilePath
        , indent :: Int
        }
    deriving (Show)

makeContext :: Args -> GenerationContext
makeContext args = GenerationContext { isEntryPoint = entryPoint args, sourceFile = inputFile, indent = 0 }
    where
        inputFile = if null $ input args then "stdin" else input args
