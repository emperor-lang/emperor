{-|
Module      : Main
Description : Entry-point for the emperor compiler
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

This is the entry-point for the compiler of the @emperor@ language.
A code-formatter may be invoked from the command-line.
For details on how this is done, see @man emperor@ or @emperor.json@ in the GitHub repository.
-}
module Main
    ( main
    ) where

import Args (Args, doFormat, entryPoint, input, outputFile, parseArgv, version)
import Control.Monad (when)
import Parser.EmperorParserWrapper (AST, parse)
import Formatter.Formatter (formatFresh)
import Logger.Logger (Loggers, makeLoggers)
import System.Exit (exitFailure, exitSuccess)
import Types.Types (TypeCheckResult(..), resolveTypes, writeHeader)

-- | Provides the entry-point
main :: IO ()
main = do
    args <- parseArgv
    (err, inf, scc, wrn) <- makeLoggers args

    when (version args) (putStrLn "emperor v1.0.0" >>= const exitSuccess)

    if input args == ""
        then wrn "No input files detected, reading from stdin"
        else inf $ "Using input file " ++ input args
    let sanitisedArguments =
            if input args == ""
                then args {input = "-"}
                else args
    parseResult <- parse (input sanitisedArguments)
    case parseResult of
        Left msg -> err msg
        Right prog -> do
            scc "Parsing done for input file"
            when (doFormat args) (output args (formatFresh prog) >>= const exitSuccess)
            typeCheck args (err, inf, scc, wrn) prog
            when
                (not (entryPoint args) && outputFile args /= "-")
                (do inf "Outputting header..."
                    writeHeader (outputFile args ++ ".eh.json.gz") prog)

typeCheck :: Args -> Loggers -> AST -> IO ()
typeCheck _ (err, inf, scc, wrn) prog = do
    inf "Checking types"
    typeResult <- resolveTypes (err, inf, scc, wrn) prog
    case typeResult of
        Fail x -> do
            err x
            exitFailure
        Pass -> scc "Type-checking passed"

output :: Args -> String -> IO ()
output args c = do
    let path = outputFile args
    if path == "-"
        then putStrLn c
        else writeFile path c
