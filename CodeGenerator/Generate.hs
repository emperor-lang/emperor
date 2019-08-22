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
module CodeGenerator.Generate (generate) where

import Args (Args)
import Parser.AST (AST)

generate :: Args -> AST -> Either String (String, String)
generate _ _ = Left "Code generation has not been implemented yet" -- TODO: This <--
