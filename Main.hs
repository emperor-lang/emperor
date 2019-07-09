-- Emperor

module Main where

import Args (parseArgv, __help__)
import Parser (parse)

main :: IO ()
main = do
    putStrLn "Hello, world!"
    args <- parseArgv
    putStrLn $ show args
    putStrLn $ "Showing help?: " ++ show (__help__ args)
