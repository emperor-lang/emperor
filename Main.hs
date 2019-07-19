-- Emperor

module Main where

import Args (parseArgv, input, verbose)
import EmperorParserWrapper (parse)
import Logger (makeLoggers)
import Formatter (formatFresh)
import Control.Monad (when)

main :: IO ()
main = do
    args <- parseArgv
    (err, inf, scc, wrn) <- makeLoggers args

    if input args == ""
        then do
            wrn "No input files detected, reading from stdin"
        else inf $ "Using input file " ++ input args

    let sanitisedArguments = if input args == ""
        then args { input = "-" }
        else args

    parseResult <- parse (input sanitisedArguments)

    case parseResult of
        Left msg    -> err msg
        Right prog  -> do
            scc $ "Compilation completed successfully, got AST: " ++ show prog
            putStrLn "==="
            putStr $ ">>>" ++ formatFresh prog ++ "<<<"
