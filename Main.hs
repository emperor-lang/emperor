-- Emperor

module Main where

import Args

main :: IO ()
main = do
    putStrLn "Hello, world!"
    args <- parseArgv
    putStrLn $ show args
    putStrLn $ "Showing help?: " ++ show (__help__ args)
