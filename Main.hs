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
module Main where

import Args (parseArgv, input, verbose)
import EmperorParserWrapper (parse)
import Logger (makeLoggers)
import Formatter (formatFresh)
import Control.Monad (when)
import Types.Types (resolveTypes)

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
            scc $ "Parsing completed successfully, got AST: " ++ show prog
            putStr $ ">>>" ++ formatFresh prog ++ "<<<"
            inf "Checking types"
            case resolveTypes prog of
                Left x  -> err $ show x
                Right r -> scc $ "Type-checking worked!\n Got:" ++ show r
