{-|
Module      : Types
Description : Wrapper for the emperor typing machinery
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

This module enables the type checking and judgement modules to be called more
easily.
-}
module Types.Types
    ( resolveTypes
    , TypeCheckResult(..)
    , writeHeader
    ) where

import Parser.AST (AST(..))
import Logger.Logger (Loggers)
import Types.Checker ((>-))
import Types.Imports.Imports (getEnvironment, writeHeader)
import Types.Results (TypeCheckResult(..))

-- | Find any problems with the typing of results and obtain types if no
-- problems are found.
resolveTypes :: Loggers -> AST -> IO TypeCheckResult
resolveTypes (err, inf, scc, wrn) (AST h is bs) = do
    inf "Getting imports..."
    r <- getEnvironment (err, inf, scc, wrn) is
    case r of
        Right g -> do
            inf $ "Got environment " ++ show g
            return $ g >- AST h is bs
        Left m -> return $ Fail m
