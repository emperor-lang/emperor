module Parser where

import EmperorLexer (runAlex)
import EmperorParser (parseEmperor, Section)

parse :: String -> IO (Either String [Section])
parse "-" = parse' getContents
parse f = parseFile f

parse' :: IO String -> IO (Either String [Section])
parse' c = do
    s <- c
    return $ parseString s

parseFile :: FilePath -> IO (Either String [Section])
parseFile name = parse' $ readFile name

parseString :: String -> Either String [Section]
parseString s = runAlex s parseEmperor