{-|
Module      : Result
Description : REsults for code generation
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

Holds the results of code generation
-}
module CodeGenerator.Results (GenerationResult(..), makeHeaderLines, makeHeaderAndBodyLines, makeBodyLines, makeConstant, getHeaderLines, getBodyLines, getConstantMapping, wrapLastBodyLine) where

import Parser.AST (Value)
import Data.Monoid ((<>))

data GenerationResult =
    GenerationResult [String] [String] [(String,Value)]
    deriving (Show)

instance Monoid GenerationResult where
    mempty = GenerationResult [] [] []
    mappend (GenerationResult hs bs cs) (GenerationResult hs' bs' cs') = GenerationResult (hs <> hs') (bs <> bs') (cs <> cs')

makeHeaderLines :: [String] -> GenerationResult
makeHeaderLines hs = GenerationResult hs [] []

makeHeaderAndBodyLines :: [String] -> GenerationResult
makeHeaderAndBodyLines ls = GenerationResult ls ls []

makeBodyLines :: [String] -> GenerationResult
makeBodyLines bs = GenerationResult [] bs []

makeConstant :: (String,Value) -> GenerationResult
makeConstant m = GenerationResult [] [] [m]

getHeaderLines :: GenerationResult -> [String]
getHeaderLines (GenerationResult hs _ _) = hs

getBodyLines :: GenerationResult -> [String]
getBodyLines (GenerationResult _ bs _) = bs

getConstantMapping :: GenerationResult -> [(String, Value)]
getConstantMapping (GenerationResult _ _ m) = m

wrapLastBodyLine :: GenerationResult -> (String -> String) -> GenerationResult
wrapLastBodyLine (GenerationResult hs bs m) f = GenerationResult hs bs' m
    where
        bs' = init bs ++ [f $ last bs]
