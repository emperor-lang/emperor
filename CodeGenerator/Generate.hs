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
module CodeGenerator.Generate ( generate ) where

import Args (Args)
import CodeGenerator.Context (makeContext)
-- import CodeGenerator.Position (generatePos)
import CodeGenerator.Results (getHeaderLines, getBodyLines, getConstantMapping)
import CodeGenerator.ToC (toC, toCString)
import Parser.AST (AST, Value)

generate :: Args -> AST -> (String,String)
generate args prog = (unlines b, unlines h ++ unlines ml)
    where
        r = toC c prog
        c = (makeContext args)
        h = getHeaderLines r
        b = getBodyLines r
        m = getConstantMapping r
        ml = mapgen <$> m
        mapgen :: (String, Value) -> String
        mapgen (k,v) = "#define " ++ toCString c k ++ " " ++ toCString c v

