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
    , readHeader
    , writeHeader
    ) where

import AST (Ident(..), ImportLocation(..), ImportType)
import Codec.Compression.GZip
import Data.Aeson (FromJSON, ToJSON, Value(Object), (.:), (.=), eitherDecode', encode, object, parseJSON, toJSON)
import Data.ByteString.Lazy (readFile, writeFile)
import Prelude hiding (readFile, writeFile)
import Types.Environment (TypeEnvironment(..))

data Header =
    Header Ident [ImportLocation] TypeEnvironment
    deriving (Show)

instance ToJSON Header where
    toJSON (Header (Ident s) ds g) = object ["name" .= s, "depends" .= ds, "environment" .= g]

instance FromJSON Header where
    parseJSON (Object v) = Header <$> (Ident <$> v .: "name") <*> v .: "depends" <*> v .: "environment"
    parseJSON _ = fail "Expected object when parsing header"

-- | Read a header from a file, obtain its type environment
readHeader :: FilePath -> IO (Either String Header)
readHeader p = do
    c <- readFile p
    let r = eitherDecode' . decompress $ c :: Either String Header
    return r

-- | Write a type environment to a file to make a header
writeHeader :: Header -> FilePath -> IO ()
writeHeader g p = writeFile p $ compress . encode $ g
