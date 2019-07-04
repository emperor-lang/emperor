-- Emperor

module Main where

import Args

main :: IO ()
main = do
    putStrLn "Hello, world!"
    args <- parseArgv
    putStrLn $ show args
    putStrLn $ show (__help__ args)
    