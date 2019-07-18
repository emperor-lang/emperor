module EmperorParserWrapper (parse, parseFile, parseString) where

import EmperorLexer (runAlex)
import EmperorParser (parseEmperor)
import AST (AST)

parse :: String -> IO (Either String AST)
parse "-" = parse' getContents
parse f = parseFile f

parse' :: IO String -> IO (Either String AST)
parse' c = do
    s <- c
    return $ parseString s

parseFile :: FilePath -> IO (Either String AST)
parseFile name = parse' $ readFile name

parseString :: String -> Either String AST
parseString s = runAlex s parseEmperor