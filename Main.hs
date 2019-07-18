-- Emperor

module Main where

import Args (parseArgv, Args, input)
import EmperorParserWrapper (parse)
import Logger (makeLoggers)
import Formatter (formatFresh)

main :: IO ()
main = do
    args <- parseArgv
    (err, inf, scc, _) <- makeLoggers args

    inf $ "Using input file " ++ input args
    
    parseResult <- parse (input args)

    case parseResult of
        Left msg    -> err msg
        Right prog  -> do
            scc $ "Compilation completed successfully, got AST: " ++ show prog
            putStrLn "==="
            putStr $ formatFresh prog
