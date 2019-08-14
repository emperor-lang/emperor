{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Imports
Description : Import handler
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

From a given file, returns the type environment specified by its interface
-}
module Types.Imports.Imports
    ( getEnvironment
    ) where

import AST (Ident(..), Import(..), ImportLocation(..), ImportType(..))
import Data.Map (filterWithKey)
import Data.Monoid ((<>))
import GHC.IO.Exception (ExitCode(..))
import Logger (Loggers)
import System.Exit (exitFailure)
import System.Process (readProcessWithExitCode)
import Types.Environment (TypeEnvironment(..))
import Types.Imports.JsonIO (Header(..), isHeaderFile, readHeader)

-- | Given a set of imports, obtain the type environment they form.
getEnvironment :: Loggers -> [Import] -> IO (Maybe TypeEnvironment)
getEnvironment _ [] = return Nothing
getEnvironment (err, inf, scc, wrn) (i:is) = do
    inf $ "Importing " ++ show i
    ti <- getEnvironment' (err, inf, scc, wrn) i
    tis <- getEnvironment (err, inf, scc, wrn) is
    return $ ti <> tis

getEnvironment' :: Loggers -> Import -> IO (Maybe TypeEnvironment)
getEnvironment' (err, inf, scc, wrn) (Import (ImportLocation t (Ident i)) mis) = do
    e <- getEnvironmentFromFile (err, inf, scc, wrn) t i
    case e of
        Just g -> return . Just $ filterEnvironment mis g
        Nothing -> return Nothing

getEnvironmentFromFile :: Loggers -> ImportType -> FilePath -> IO (Maybe TypeEnvironment)
getEnvironmentFromFile _ Local _ = error "Cannot import local files!"
getEnvironmentFromFile (err, inf, scc, wrn) Global p = do
    inf "Getting install location"
    (c, stdoutContent, stderrContent) <- readProcessWithExitCode "emperor-setup" ["-L"] ""
    let libraryInstallationDirectory = init stdoutContent
    if c == ExitSuccess
        then scc $ "Got import location: " ++ libraryInstallationDirectory
        else do
            err ("Could not obtain library information from emperor-setup! Is this installed?")
            err stderrContent

    let headerLocation = libraryInstallationDirectory ++ p ++ ".json.gz"
    inf $ "Operating on header " ++ headerLocation

    e <- isHeaderFile (err, inf, scc, wrn) headerLocation
    if not e
        then do
            err $ "Could not find header file from request for " ++ headerLocation
            return Nothing
        else do
            headerJson <- readHeader (err, inf, scc, wrn) $ headerLocation
            case headerJson of
                Left x -> do
                    err x
                    exitFailure
                Right (Header _ _ g) -> return $ Just g

filterEnvironment :: Maybe [Ident] -> TypeEnvironment -> TypeEnvironment
filterEnvironment Nothing x = x
filterEnvironment (Just is) (TypeEnvironment e) = TypeEnvironment $ filterWithKey (\k _ -> Ident k `elem` is) e
