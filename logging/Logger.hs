module Logger (makeLoggers, Loggers) where

import Args (Args, verbose, useColour, noUseColour)
import System.IO (hPutStrLn, stderr)

data LogType = Info
             | Warning
             | Error
             | Success

type Loggers = (Logger, Logger, Logger, Logger)
type Logger = String -> IO ()

makeLoggers :: Args -> (Logger, Logger, Logger, Logger)
makeLoggers args = (logError, logInfo, logSuccess, logWarning)
    where
        logError = makeLogger args Error
        logInfo = makeVerboseLogger args Info
        logSuccess = makeVerboseLogger args Success
        logWarning = makeVerboseLogger args Warning

trivialLogger :: Logger
trivialLogger _ = return ()

makeVerboseLogger :: Args -> LogType -> Logger
makeVerboseLogger a t = if verbose a
    then 
        makeLogger a t
    else
        trivialLogger

makeLogger :: Args -> LogType -> Logger
makeLogger a t = hPutStrLn stderr . colouriseLog a t

-- logError :: String -> IO ()
-- logError = makeLogger Error

-- logWarning :: String -> IO ()
-- logWarning = makeLogger Warning

-- logInfo :: String -> IO ()
-- logInfo = makeLogger Info

-- logSuccess :: String -> IO ()
-- logSuccess = makeLogger Success

-- class Colours:
-- 	BOLD:str = '\033[01m'
-- 	ERROR:str = '\033[01;31m'
-- 	INFO:str = '\033[00;34m'
-- 	NORMAL:str = '\033[0m'
-- 	SUCCESS:str = '\033[00;32m'
-- 	WARNING:str = '\033[01;93m'

colouriseLog :: Args -> LogType -> String -> String
colouriseLog args t m = messageHeader t ++ " " ++ m
    where
        messageHeader :: LogType -> String
        messageHeader t' = colour t' ++ messageHeaderText t' ++ "\x1b[00;00m"

        messageHeaderText :: LogType -> String
        messageHeaderText Info      = "info"    ++ ":"
        messageHeaderText Warning   = "warning" ++ ":"
        messageHeaderText Error     = "error"   ++ ":"
        messageHeaderText Success   = "success" ++ ":"
        
        colour :: LogType -> String
        colour Info     = useANSI args "\x1b[01;34m"
        colour Warning  = useANSI args "\x1b[01;93m"
        colour Error    = useANSI args "\x1b[01;31m"
        colour Success  = useANSI args "\x1b[01;32m"

        useANSI :: Args -> String -> String
        useANSI a s = if not (noUseColour a) || useColour a then s else ""