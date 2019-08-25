{-|
Module      : ToC
Description : Generate C Code
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

Handles the generation of C body code, headers and appropriate directives.
-}
module CodeGenerator.ToC where

import CodeGenerator.Context
    ( GenerationContext
    , indent
    , isEntryPoint
    , makeContext
    , sourceFile
    )
import CodeGenerator.Results (GenerationResult, makeHeaderLines, makeHeaderAndBodyLines, makeBodyLines, makeConstant)
import Data.Monoid ((<>))
import Parser.AST
    ( Assignment(..)
    , AST(..)
    , BodyBlock(..)
    , BodyLine(..)
    , Call(..)
    , Expr(..)
    , FunctionDef(..)
    , FunctionTypeDef(..)
    , Ident(..)
    , Import(..)
    , ImportLocation(..)
    , ImportType(..)
    , ModuleHeader(..)
    , ModuleItem(..)
    , Queue(..)
    , SwitchCase(..)
    , TypeComparison(..)
    , Value(..)
    )
import Types.Results (EmperorType(..), getTypeList)

class ToC a where
    toC :: GenerationContext -> a -> GenerationResult

instance ToC AST where
    toC c (AST m is ms) = toC c m <> (foldr1 (<>) $ toC c <$> is) <> (foldr1 (<>) $ toC c <$> ms)

instance ToC ModuleHeader where
    toC c (Module i _) = makeHeaderAndBodyLines ["#include \"" ++ sourceFile c ++ ".h\""]

instance ToC Import where
    toC c (Import l _ _) = toC c l

instance ToC ImportLocation where
    toC _ (ImportLocation Global i _) = makeHeaderLines ["#include <" ++ show i ++ ">"]
    toC _ (ImportLocation Local i _) = makeHeaderLines ["#include \"" ++ show i ++ "\""]

instance ToC ModuleItem where
    toC _ Component{} = error $ "Components have not been implemented yet, and this should have been caught at the type-checking stage??"
    toC _ TypeClass{} = error $ "TypeClasses have not been implemented yet, and this should have been caught at the type-checking stage??"
    toC c (FunctionItem f _) = toC c f

instance ToC FunctionDef where
    toC c (FunctionDef (FunctionTypeDef i t p) is bs p') =
        where
            inputTypes = init $ getTypeList t
            returnType = last $ getTypeList t

            prototype = toC c returnType ++ " " ++ show i ++ "(" ++ ");"
