{-|
Module      : Position
Description : #line directive generator
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

Creates the #line directives for items with a position in code.
-}
module CodeGenerator.Position
    ( GetPos
    , generatePos
    ) where

import Parser.EmperorLexer (AlexPosn(..))
import Parser.Position (GetPos, getPos)
import CodeGenerator.Context (GenerationContext, sourceFile, annotation)

-- | Get the line position directive
generatePos :: GetPos a => GenerationContext -> a -> String
generatePos c x = if annotation c >= 1
        then "#line " ++ show p ++ " " ++ (show . sourceFile) c
        else ""
  where
    AlexPn _ p _ = getPos x
