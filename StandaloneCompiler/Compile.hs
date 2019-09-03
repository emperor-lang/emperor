module StandaloneCompiler.Compile
    ( nativeCompile
    ) where

import Args (Args, input, outputFile)
import Logger.Logger (Loggers)
import System.Exit (ExitCode(ExitSuccess), exitFailure)
import System.IO (hPutStr, stderr)
import System.Process (readProcessWithExitCode)

nativeCompile :: Args -> Loggers -> (String, String) -> IO ()
nativeCompile args (err, inf, wrn, scc) (b, h) = do
    let prog = h ++ '\n' : b
    inf "Compiling natively"
    cfsr <- getCflags (err, inf, wrn, scc)
    case cfsr of
        Left m -> do
            err "Failed to get c flags from emperor-setup"
            hPutStr stderr m
            exitFailure
        Right cfs -> do
            scc "Got C flags"
            lsr <- getLibs (err, inf, wrn, scc)
            case lsr of
                Left m -> do
                    err "Failed to get libraries from emperor-setup"
                    hPutStr stderr m
                    exitFailure
                Right ls -> do
                    scc "Got libraries"
                    let outFile =
                            if outputFile args /= "-"
                                then outputFile args
                                else if input args /= ""
                                         then input args
                                         else "a.out"
                    inf $ "Running gcc, outputting to " ++ outFile
                    (c, outs, errs) <-
                        readProcessWithExitCode "gcc-8" (words cfs ++ ["-xc", "-", "-o", outFile] ++ words ls) prog
                    if c /= ExitSuccess
                        then do
                            err "GCC produced the following error(s)"
                            hPutStr stderr errs
                            exitFailure
                        else do
                            scc "C compilation complete"
                            putStr outs

getCflags :: Loggers -> IO (Either String String)
getCflags (_, inf, _, _) = do
    inf "Getting C flags from emperor-setup"
    getFromSetup ["--cflags", "--entry-point"]

getLibs :: Loggers -> IO (Either String String)
getLibs (_, inf, _, _) = do
    inf "Getting libraries from emperor-setup"
    getFromSetup ["--libs"]

getFromSetup :: [String] -> IO (Either String String)
getFromSetup fs = do
    (c, out, err) <- readProcessWithExitCode "emperor-setup" fs ""
    if c /= ExitSuccess
        then return $ Left err
        else return . Right $ init out
