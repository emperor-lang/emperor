{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : JsonIO
Description : Input/output definitions for emperor header files
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

This defines the machinery to encode and decode the type environments exported 
by modules in JSON. Common I/O operations are also provided
-}
module Types.Imports.JsonIO
    ( Header(..)
    , isHeaderFile
    , readHeader
    , writeHeader
    ) where

import AST (Ident(..), ImportLocation(..))
import Codec.Compression.GZip (compress, decompress)
import Data.Aeson (FromJSON, ToJSON, Value(Object), (.:), (.=), eitherDecode', encode, object, parseJSON, toJSON)
import Data.ByteString.Lazy (readFile, writeFile)
import Logger (Loggers)
import Prelude hiding (readFile, writeFile)
import System.Directory (doesFileExist)
import Types.Environment (TypeEnvironment(..))

-- | Defines module header file contents
data Header =
    Header Ident [ImportLocation] TypeEnvironment
    deriving (Show)

instance ToJSON Header where
    toJSON (Header (Ident s) ds g) = object ["name" .= s, "depends" .= ds, "environment" .= g]

instance FromJSON Header where
    parseJSON (Object v) = Header <$> (Ident <$> v .: "name") <*> v .: "depends" <*> v .: "environment"
    parseJSON _ = fail "Expected object when parsing header"

-- | Checks whether a given header file exists
isHeaderFile :: Loggers -> FilePath -> IO Bool
isHeaderFile (_, inf, _, _) p = do
    inf $ "Checking for header file " ++ p
    doesFileExist p

-- | Read a header from a file, obtain its type environment
readHeader :: Loggers -> FilePath -> IO (Either String Header)
readHeader (_, inf, _, _) p = do
    inf $ "Reading from header file " ++ show p
    c <- readFile p
    inf "Decompressing contents"
    return $ eitherDecode' . decompress $ c 

-- | Write a type environment to a file to make a header
writeHeader :: Header -> FilePath -> IO ()
writeHeader g p = writeFile p $ compress . encode $ g
