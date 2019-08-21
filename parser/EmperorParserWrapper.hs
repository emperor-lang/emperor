{-|
Module      : EmperorParserWrapper
Description : Wrapper for the Emperor parser and lexer
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

This forms a wrapper to make the Emperor parser and lexer easier to use.
It extends functionality by allowing the format of strings, arbitrary files on disk, and @stdin@ when the file "-" is used.
-}
module EmperorParserWrapper
    ( AST
    , parse
    , parseFile
    , parseString
    ) where

import AST (AST)
import EmperorLexer (runAlex)
import EmperorParser (parseEmperor)

-- | Parse a file of "-" for @stdin@
parse :: String -> IO (Either String AST)
parse "-" = parse' getContents
parse f = parseFile f

-- | Parse an IO String (e.g. raw result of reading a file)
parse' :: IO String -> IO (Either String AST)
parse' c = do
    s <- c
    return $ parseString s

-- | Parse an arbitrary file on disk
parseFile :: FilePath -> IO (Either String AST)
parseFile name = parse' $ readFile name

-- | Parse a given string
parseString :: String -> Either String AST
parseString s = runAlex s parseEmperor
