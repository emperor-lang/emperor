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
    ( generatePos
    ) where

import Parser.EmperorLexer (AlexPosn(..))
import Parser.Position (GetPos, getPos)

-- | Get the line position directive
generatePos :: GetPos a => FilePath -> a -> String
generatePos p x = "#line " ++ show q ++ " " ++ show p
  where
    AlexPn _ q _ = getPos x
