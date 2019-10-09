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
module Parser.EmperorParserWrapper
    ( AST
    , parse
    , parseFile
    , parseString
    ) where

import           Errors.Errors        (ErrorResult (..), makeErrorResult)
import           Parser.AST           (AST)
import           Parser.EmperorLexer  (runAlex)
import           Parser.EmperorParser (parseEmperor)

-- | Parse a file of "-" for @stdin@
parse :: String -> IO (ErrorResult AST)
parse "-" = parse' getContents
parse f   = parseFile f

-- | Parse an IO String (e.g. raw result of reading a file)
parse' :: IO String -> IO (ErrorResult AST)
parse' c = do
    s <- c
    return $ parseString s

-- | Parse an arbitrary file on disk
parseFile :: FilePath -> IO (ErrorResult AST)
parseFile name = parse' $ readFile name

-- | Parse a given string
parseString :: String -> ErrorResult AST
parseString s = case runAlex s parseEmperor of
    Right x -> Dex x
    Left m  -> makeErrorResult m Nothing
