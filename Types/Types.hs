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
module Types.Types (resolveTypes, TypeCheckResult(..)) where

import AST (AST(..), Import)
import Logger (Loggers)
import Types.Imports.Imports (getEnvironment)
import Types.Environment (TypeEnvironment)
import Types.Results (TypeCheckResult(..))

-- | Find any problems with the typing of results and obtain types if no 
-- problems are found.
resolveTypes :: Loggers -> AST -> IO TypeCheckResult
resolveTypes (err, inf, scc, wrn) a = do
    let imports = getImports a
    inf "Getting imports..."
    g <- getEnvironment (err, inf, scc, wrn) imports
    inf $ "Got environment " ++ show g
    return $ Fail "Not yet implemented!" 

getImports :: AST -> [Import]
getImports (AST _ is _) = is
