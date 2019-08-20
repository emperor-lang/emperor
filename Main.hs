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
module Main (main) where

import Args (Args, parseArgv, input, entryPoint, doFormat, outputFile)
import Control.Monad (when)
import EmperorParserWrapper (AST, parse)
import Logger (Loggers, makeLoggers)
import Formatter (formatFresh)
import System.Exit (exitSuccess, exitFailure)
import Types.Types (resolveTypes, TypeCheckResult(..), writeHeader)

-- | Provides the entry-point
main :: IO ()
main = do
    args <- parseArgv
    (err, inf, scc, wrn) <- makeLoggers args

    if input args == ""
        then wrn "No input files detected, reading from stdin"
        else inf $ "Using input file " ++ input args

    let sanitisedArguments = if input args == ""
        then args { input = "-" }
        else args

    parseResult <- parse (input sanitisedArguments)        
        
    case parseResult of
        Left msg    -> err msg
        Right prog  -> do
            scc "Parsing done for input file"
            when (doFormat args) (output args (formatFresh prog) >>= const exitSuccess)
            typeCheck args (err, inf, scc, wrn) prog
            when (not (entryPoint args) && outputFile args /= "-") (
                    do
                        inf "Outputting header..."
                        writeHeader (outputFile args ++ ".eh.json.gz") prog
                )

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