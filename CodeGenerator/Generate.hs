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
module CodeGenerator.Generate
    ( generate
    , generateHeadless
    ) where

import Args (Args, input)
import CodeGenerator.Context (GenerationContext, makeContext, sourceFile)
import CodeGenerator.Position (generatePos)
import CodeGenerator.Results (GenerationResult)
import CodeGenerator.ToC (toC)
import Parser.AST (AST)

generate :: Args -> AST -> (String,String)
generate args prog =
    where
        r = toC (makeContext args) prog
