{-|
Module      : Generate
Description : Code generator
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

This module handles the generation of C code from an AST. Output is given as a
pair of strings, the first containing C to execute, and the second containing
its C header.
-}
module CodeGenerator.Generate
    ( generate
    , generateHeadless
    ) where

import Args (Args, input)
import CodeGenerator.Context (GenerationContext, makeContext, sourceFile)
import CodeGenerator.Position (generatePos)
import Parser.AST
    ( AST(..)
    , Assignment(..)
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

generate :: Args -> AST -> Either String (String, String)
generate args prog = (,) <$> generateHeadless args prog <*> generateHeader args prog

generateHeadless :: Args -> AST -> Either String String
generateHeadless args p = Right $ genCode c p
    where
        c = makeContext args

generateHeader :: Args -> AST -> Either String String
generateHeader args (AST _ _ bs) = Right . unlines $ (generatePos (sourceFile c)) <$> bs
    where
        c = makeContext args

class GenHeadlessCode a where
    genCode :: GenerationContext -> a -> String
    genCode c x = unlines $ genCodeLines c x
    genCodeLines :: GenerationContext -> a -> [String]

instance GenHeadlessCode AST where
    genCodeLines c (AST m is bs) = generatePos (sourceFile c) (AST m is bs) : genCodeLines c is ++ genCodeLines c bs

instance GenHeadlessCode a => GenHeadlessCode [a] where
    genCodeLines c as = foldr1 (++) (genCodeLines c <$> as)

instance GenHeadlessCode Import where 
    genCodeLines _ _ = [""]
