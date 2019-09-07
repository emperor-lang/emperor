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
    , annotation
    , destFile
    , exposedIdents
    , indent
    , isEntryPoint
    , makeContext
    , makeIndent
    , moreIndent
    , nativeCompile
    , sourceFile
    ) where

import qualified Args (Args, entryPoint, input, outputFile, annotateSource, toCOnly)
import Parser.AST (Ident)

data GenerationContext =
    GenerationContext
        { isEntryPoint :: Bool
        , sourceFile :: FilePath
        , destFile :: FilePath
        , indent :: Int
        , annotation :: Int
        , nativeCompile :: Bool
        , exposedIdents :: Maybe [Ident]
        }
    deriving (Show)

makeContext :: Args.Args -> GenerationContext
makeContext args = GenerationContext
        { isEntryPoint = Args.entryPoint args
        , sourceFile = inputFile
        , indent = 0
        , destFile = outFile
        , annotation = Args.annotateSource args
        , nativeCompile = (not . Args.toCOnly) args
        , exposedIdents = Nothing
        }
    where
        inputFile = if null $ Args.input args then "stdin" else Args.input args
        outFile = if Args.outputFile args == "-" then "stdout" else Args.outputFile args

makeIndent :: GenerationContext -> String
makeIndent c = replicate (indent c) '\t'

moreIndent :: GenerationContext -> GenerationContext
moreIndent c = c { indent = indent c + 1 }
