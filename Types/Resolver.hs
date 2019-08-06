{-|
Module      : Resolver
Description : Type resolver for emperor
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

This module defines typing judgements for the AST. Types are found and checked 
for compatibility.
-}
module Types.Resolver
    ( resolve
    ) where

import AST
    ( AST(..)
    , Assignment(..)
    , BodyBlock(..)
    , BodyLine(..)
    , BodyLineContent(..)
    , Expr(..)
    , Ident(..)
    , Import(..)
    , ImportLocation(..)
    , ImportType(..)
    , ModuleHeader(..)
    , PartialCall(..)
    , Queue(..)
    , SwitchCase(..)
    , Tabs(..)
    , Value(..)
    , getPurity
    )
import Types.Environment (TypeEnvironment, newTypeEnvironment)
import Types.Judger (Typable, (|>))
import Types.Results (EmperorType(..), Purity(..), TypeCheckResult(..), TypeJudgementResult(..))
import Types.TypedAST (TypedAST)

-- | Given an AST, resolve all types within it and return the type environment of its interface
resolve :: AST -> Either String (TypedAST, TypeEnvironment)
resolve = resolve' newTypeEnvironment

resolve' :: TypeEnvironment -> AST -> Either String (TypedAST, TypeEnvironment)
resolve' _ _ = Left "Type resolution has not been implemented yet"
