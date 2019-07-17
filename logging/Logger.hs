module Logger (makeLoggers, Loggers) where

import Args (Args, verbose, useColour, noUseColour)
import System.IO (hPutStrLn, stderr)
import System.Console.ANSI (hSupportsANSIColor)

data LogType = Info
             | Warning
             | Error
             | Success

type Loggers = (Logger, Logger, Logger, Logger)
type Logger = String -> IO ()

makeLoggers :: Args -> IO (Logger, Logger, Logger, Logger)
makeLoggers args = do
    logError <- makeLogger args Error
    logInfo <- makeVerboseLogger args Info
    logSuccess <- makeVerboseLogger args Success
    logWarning <- makeVerboseLogger args Warning
    return (logError, logInfo, logSuccess, logWarning)

trivialLogger :: Logger
trivialLogger _ = return ()

makeVerboseLogger :: Args -> LogType -> IO Logger
makeVerboseLogger a t = if verbose a
    then 
        makeLogger a t
    else
        return trivialLogger

makeLogger :: Args -> LogType -> IO Logger
makeLogger a t = do
    colourCompat <- hSupportsANSIColor stderr
    return $ hPutStrLn stderr . colouriseLog colourCompat a t

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
                useANSIColour = (colourSupported && not ((useColour a) && (not $ noUseColour a))) || (useColour a)
                    -- if useColour a
                    -- then True
                    -- else if not (noUseColour a)
                    --     then False
                    --     else colourSupported 
