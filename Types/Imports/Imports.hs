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
    , getLocalEnvironment
    , Types.Imports.Imports.writeHeader
    ) where

import AST (AST(..), Ident(..), Import(..), ImportLocation(..), ImportType(..), ModuleHeader(..), ModuleItem(..), FunctionDef(..), FunctionTypeDef(..))
import Data.Monoid ((<>))
import GHC.IO.Exception (ExitCode(..))
import Logger (Loggers)
import System.Exit (exitFailure)
import System.Process (readProcessWithExitCode)
import Types.Environment (TypeEnvironment(..), filterEnvironment, insert, newTypeEnvironment)
import Types.Imports.JsonIO (Header(..), isHeaderFile, readHeader, writeHeader)

writeHeader :: FilePath -> AST -> IO ()
writeHeader f a = do
        let g = getLocalEnvironment a
        let h = generateHeader g a
        Types.Imports.JsonIO.writeHeader h f
    where
        generateHeader :: TypeEnvironment -> AST -> Header
        generateHeader g' (AST (Module i) is _) = Header i (loc <$> is) g'
            where
                loc :: Import -> ImportLocation
                loc (Import l _) = l

-- | Obtain the type environment created by the content of the module
getLocalEnvironment :: AST -> TypeEnvironment
getLocalEnvironment (AST _ _ as) = getLocalEnvironment' as
    where
        getLocalEnvironment' :: [ModuleItem] -> TypeEnvironment
        getLocalEnvironment' [] = newTypeEnvironment
        getLocalEnvironment' (m:ms) = case m of
            Component _ _ _ -> error $ "Components have not been implemented for type-checking (and this should have been stopped sooner..." --  getLocalEnvironment ms
            TypeClass _ _ _ -> error $ "Classes have not been implemented for type-checking (and this should have been stopped sooner..." --  getLocalEnvironment ms
            FunctionItem (FunctionDef (FunctionTypeDef (Ident i) t) _ _) -> insert i t $ getLocalEnvironment' ms

-- | Given a set of imports, obtain the type environment they form.
getEnvironment :: Loggers -> [Import] -> IO (Maybe TypeEnvironment)
getEnvironment _ [] = return $ Just newTypeEnvironment
getEnvironment (err, inf, scc, wrn) (i:is) = do
    inf $ "Importing " ++ show i
    ti <- getEnvironment' (err, inf, scc, wrn) i
    tis <- getEnvironment (err, inf, scc, wrn) is
    return $ ti <> tis

getEnvironment' :: Loggers -> Import -> IO (Maybe TypeEnvironment)
getEnvironment' (err, inf, scc, wrn) (Import (ImportLocation t (Ident i)) mis) = do
    e <- getEnvironmentFromFile (err, inf, scc, wrn) t i
    case e of
        Just g -> case mis of
            Just is -> return . Just $ filterEnvironment (`elem` ((\(Ident i') -> i') <$> is)) g
            Nothing -> return . Just $ g
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
