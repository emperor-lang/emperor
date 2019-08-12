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
import Types.Environment (TypeEnvironment(..), newTypeEnvironment)
import Types.Imports.JsonIO (Header(..), readHeader)

getEnvironment :: Loggers -> [Import] -> IO TypeEnvironment
getEnvironment _ [] = return newTypeEnvironment
getEnvironment (err, inf, scc, wrn) (i:is) = do
    inf $ "Importing " ++ show i
    ti <- getEnvironment' (err, inf, scc, wrn) i
    tis <- getEnvironment (err, inf, scc, wrn) is
    return $ ti <> tis

getEnvironment' :: Loggers -> Import -> IO TypeEnvironment
getEnvironment' (err, inf, scc, wrn) (Import (ImportLocation t (Ident i)) mis) = do
    e <- getEnvironmentFromFile (err, inf, scc, wrn) t i
    return $ filterEnvironment mis e

getEnvironmentFromFile :: Loggers -> ImportType -> FilePath -> IO TypeEnvironment
getEnvironmentFromFile _ Local _ = error "Cannot import local files!"
getEnvironmentFromFile (err, inf, scc, _) Global p = do
    inf "Getting install location"
    (c, libraryInstallationDirectory, stderrContent) <- readProcessWithExitCode "emperor-setup" ["-L"] ""
    if c == ExitSuccess
        then scc $ "Imported " ++ p
        else do
            err ("Could not import <" ++ p ++ ">!")
            err stderrContent
    putStrLn libraryInstallationDirectory
    print c
    putStrLn stderrContent
    headerJson <- readHeader p
    case headerJson of
        Left x -> do
            err x
            exitFailure
        Right (Header _ _ g) -> return g

filterEnvironment :: Maybe [Ident] -> TypeEnvironment -> TypeEnvironment
filterEnvironment Nothing x = x
filterEnvironment (Just is) (TypeEnvironment e) = TypeEnvironment $ filterWithKey (\k _ -> Ident k `elem` is) e
