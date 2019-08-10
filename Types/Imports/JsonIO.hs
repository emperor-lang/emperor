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
module Types.Imports.JsonIO (readHeader, writeHeader) where

import Data.Aeson (eitherDecode', encode)
import Data.ByteString.Lazy (readFile, writeFile)
import Prelude hiding (readFile, writeFile)
import Types.Environment (TypeEnvironment(..))

-- | Read a header from a file, obtain its type environment
readHeader :: FilePath -> IO (Either String TypeEnvironment)
readHeader p = do
    c <- readFile p
    let r = eitherDecode' c :: Either String TypeEnvironment
    return r

-- | Write a type environment to a file to make a header
writeHeader :: TypeEnvironment -> FilePath -> IO ()
writeHeader g p = writeFile p $ encode g
