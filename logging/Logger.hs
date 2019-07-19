{-|
Module      : Logger
Description : Suppressable loggers with optional colour output
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

Used to create loggers which output to @stderr@ using ANSI formatted output.
-}
module Logger (makeLoggers, Loggers) where

import Args (Args, verbose, useColour, noUseColour)
import System.IO (hPutStrLn, stderr)
import System.Console.ANSI (hSupportsANSIColor)

data LogType = Info
             | Warning
             | Error
             | Success

-- | Defines a set of logger functions.
-- From left to right these are: errorLogger, infoLogger, successLogger, warningLogger
type Loggers = (Logger, Logger, Logger, Logger)
type Logger = String -> IO ()

-- | Make a set of loggers according to the command-line argument. (Specifically 
-- whether to print verbose output and whether to override the heuristic-based 
-- method for checking colour-compatibility.)
makeLoggers :: Args -> IO (Logger, Logger, Logger, Logger)
makeLoggers args = do
    colourCompat <- hSupportsANSIColor stderr

    let logError = makeLogger colourCompat args Error
    let logInfo = makeVerboseLogger colourCompat args Info
    let logSuccess = makeVerboseLogger colourCompat args Success
    let logWarning = makeLogger colourCompat args Warning
    return (logError, logInfo, logSuccess, logWarning)

trivialLogger :: Logger
trivialLogger _ = return ()

makeVerboseLogger :: Bool -> Args -> LogType -> Logger
makeVerboseLogger c a t = if verbose a
    then 
        makeLogger c a t
    else
        trivialLogger

makeLogger :: Bool -> Args -> LogType -> Logger
makeLogger c a t = hPutStrLn stderr . colouriseLog c a t

colouriseLog :: Bool -> Args -> LogType -> String -> String
colouriseLog c args t m = messageHeader c t ++ " " ++ m
    where
        messageHeader :: Bool -> LogType -> String
        messageHeader c' t' = colour c' t' ++ messageHeaderText t' ++ "\x1b[00;00m"

        messageHeaderText :: LogType -> String
        messageHeaderText Info      = "info"    ++ ":"
        messageHeaderText Warning   = "warning" ++ ":"
        messageHeaderText Error     = "error"   ++ ":"
        messageHeaderText Success   = "success" ++ ":"
        
        colour :: Bool -> LogType -> String
        colour c'' Info     = useANSI c'' args "\x1b[01;34m"
        colour c'' Warning  = useANSI c'' args "\x1b[01;93m"
        colour c'' Error    = useANSI c'' args "\x1b[01;31m"
        colour c'' Success  = useANSI c'' args "\x1b[01;32m"

        useANSI :: Bool -> Args -> String -> String
        useANSI colourSupported a s = if useANSIColour then s else ""
            where 
                useANSIColour :: Bool
                useANSIColour = useColour a || (not (noUseColour a) && colourSupported)
