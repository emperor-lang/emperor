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

import Data.Monoid ((<>))
import GHC.IO.Exception (ExitCode(..))
import Logger.Logger (Loggers)
import Parser.AST
    ( AST(..)
    , FunctionDef(..)
    , FunctionTypeDef(..)
    , Ident(..)
    , Import(..)
    , ImportLocation(..)
    , ImportType(..)
    , ModuleHeader(..)
    , ModuleItem(..)
    )
import System.Process (readProcessWithExitCode)
import Types.Environment (TypeEnvironment(..), filterEnvironment, has, insert, newTypeEnvironment)
import Types.Imports.JsonIO (Header(..), isHeaderFile, readHeader, writeHeader)

-- | Write a header to a file
writeHeader :: FilePath -> AST -> IO ()
writeHeader f a = do
    let r = getLocalEnvironment a
    case r of
        Right g -> do
            let h = generateHeader g a
            Types.Imports.JsonIO.writeHeader h f
        Left m -> error m
  where
    generateHeader :: TypeEnvironment -> AST -> Header
    generateHeader g' (AST (Module i _) is _) = Header i (loc <$> is) g'
      where
        loc :: Import -> ImportLocation
        loc (Import l _ _) = l

-- | Obtain the type environment created by the content of the module
getLocalEnvironment :: AST -> Either String TypeEnvironment
getLocalEnvironment (AST _ _ as) = getLocalEnvironment' as
  where
    getLocalEnvironment' :: [ModuleItem] -> Either String TypeEnvironment
    getLocalEnvironment' [] = Right newTypeEnvironment
    getLocalEnvironment' (m:ms) =
        case m of
            Component {} ->
                error
                    "Components have not been implemented for type-checking (and this should have been stopped sooner..." --  getLocalEnvironment ms
            TypeClass {} ->
                error "Classes have not been implemented for type-checking (and this should have been stopped sooner..." --  getLocalEnvironment ms
            FunctionItem (FunctionDef (FunctionTypeDef (Ident i _) t _) _ _ _) _ -> case getLocalEnvironment' ms of
                Right g -> if g `has` i
                    then Left $ "Function " ++ show i ++ " already exists in the current scope"
                    else Right $ insert i t g
                x -> x

-- | Given a set of imports, obtain the type environment they form.
getEnvironment :: Loggers -> [Import] -> IO (Either String TypeEnvironment)
getEnvironment _ [] = return $ Right newTypeEnvironment
getEnvironment (err, inf, scc, wrn) (i:is) = do
    inf $ "Importing " ++ show i
    tir <- getEnvironment' (err, inf, scc, wrn) i
    case tir of
        Right ti -> do
            tisr <- getEnvironment (err, inf, scc, wrn) is
            case tisr of
                Right tis -> return . Right $ ti <> tis
                x -> return x
        x -> return x

getEnvironment' :: Loggers -> Import -> IO (Either String TypeEnvironment)
getEnvironment' (err, inf, scc, wrn) (Import (ImportLocation t (Ident i _) _) mis _) = do
    e <- getEnvironmentFromFile (err, inf, scc, wrn) t i
    case e of
        Right g ->
            case mis of
                Just is ->
                    if all (g `has`) $ (\(Ident i' _) -> i') <$> is
                        then return . Right $ filterEnvironment (`elem` ((\(Ident i' _) -> i') <$> is)) g
                        else return . Left $
                             "Environment of " ++
                             show i ++
                             " does not contain " ++ show (head $ filter (\(Ident i' _) -> not $ g `has` i') is)
                Nothing -> return . Right $ g
        Left m -> return $ Left m

getEnvironmentFromFile :: Loggers -> ImportType -> FilePath -> IO (Either String TypeEnvironment)
getEnvironmentFromFile l Local p = getEnvironmentFromFile' l $ "./" ++ p
getEnvironmentFromFile (err, inf, scc, wrn) Global p = do
    inf "Getting install location"
    (c, stdoutContent, stderrContent) <- readProcessWithExitCode "emperor-setup" ["-L"] ""
    let libraryInstallationDirectory = init stdoutContent
    if c /= ExitSuccess
        then return . Left $
             "Could not obtain library information from emperor-setup! Is this installed? Got error output:\n" ++
             stderrContent
        else do
            scc $ "Got import location: " ++ libraryInstallationDirectory
            getEnvironmentFromFile' (err, inf, scc, wrn) $ libraryInstallationDirectory ++ p

getEnvironmentFromFile' :: Loggers -> FilePath -> IO (Either String TypeEnvironment)
getEnvironmentFromFile' (err, inf, scc, wrn) p = do
    let headerLocation = p ++ ".eh.json.gz"
    inf $ "Operating on header " ++ p
    e <- isHeaderFile (err, inf, scc, wrn) headerLocation
    if not e
        then return . Left $
             "Could not find header file from request for " ++ headerLocation ++ " has it definitely been compiled?"
        else do
            headerJson <- readHeader (err, inf, scc, wrn) headerLocation
            case headerJson of
                Right (Header _ _ g) -> return $ Right g
                Left m -> return $ Left m
