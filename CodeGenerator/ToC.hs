{-# LANGUAGE FlexibleInstances #-}
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
module CodeGenerator.ToC (ToC, ToCString, toC, toCString) where

import CodeGenerator.Context
    ( GenerationContext
    , makeIndent
    , moreIndent
    , sourceFile
    , destFile
    )
import CodeGenerator.Position (GetPos, generatePos)
import CodeGenerator.Results (GenerationResult, makeHeaderLines, makeHeaderAndBodyLines, makeBodyLines, wrapLastBodyLine)--, makeConstant)
import Data.Char (toUpper)
import Data.List (intercalate)
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
    , Value(..)
    )
import Types.Results (EmperorType(..), getTypeList)

class ToC a where
    toC :: GenerationContext -> a -> GenerationResult

class ToCString a where
    toCString :: GenerationContext -> a -> String

instance ToC AST where
    toC c (AST m is ms) = makeHeaderLines ["#ifndef " ++ includeGuard, "#define " ++ includeGuard, ""] <> toC c m <> (foldr (<>) mempty $ toC c <$> is) <> makeHeaderLines ["", "#include <banned.h>", ""] <> (foldr (<>) mempty $ toC c <$> ms) <> makeHeaderLines ["", "#endif /* " ++ includeGuard ++ " */"]
        where
            includeGuard = "__" ++ (toUpper <$> ((sanitise .  sourceFile) c)) ++ "_H_"
            sanitise :: String -> String
            sanitise s = if head s `elem` ['.', '/']
                    then sanitise $ tail s
                    else replace <$> s
              where
                replace :: Char -> Char
                replace '.' = '_'
                replace '/' = '$'
                replace x = x

instance ToC ModuleHeader where
    toC c (Module i p) = generatePosLines makeHeaderAndBodyLines c (Module i p) <> makeBodyLines ["#include \"" ++ destFile c ++ ".h\""] <> makeHeaderAndBodyLines ["// This is module " ++ show (toCString c i) ++ " generated from " ++ (show . sourceFile) c ++ " by emperor"] <> makeHeaderLines ["#pragma GCC dependency " ++ (show . sourceFile) c, "", "#include <OS.h>"]

instance ToC Import where
    toC c (Import l _ _) = toC c l

instance ToC ImportLocation where
    toC c (ImportLocation Global i p) = generatePosLines makeHeaderLines c (ImportLocation Global i p) <> makeHeaderLines ["#include <" ++ toCString c i ++ ".h>"]
    toC c (ImportLocation Local i p) = generatePosLines makeHeaderLines c (ImportLocation Local i p) <> makeHeaderLines ["#include \"" ++ toCString c i ++ ".h\""]

instance ToC ModuleItem where
    toC _ Component{} = error $ "Components have not been implemented yet, and this should have been caught at the type-checking stage??"
    toC _ TypeClass{} = error $ "TypeClasses have not been implemented yet, and this should have been caught at the type-checking stage??"
    toC c (FunctionItem f p) = makeBodyLines [""] <> generatePosLines makeHeaderAndBodyLines c (FunctionItem f p) <> toC c f

instance ToC FunctionDef where
    toC c (FunctionDef (FunctionTypeDef i t _) is bs _) = prototype <> makeBodyLines [returnString ++ " " ++ toCString c i ++ "(" ++ paramSig ++ ")", "{"] <> body <> makeBodyLines ["}"]
        where
            paramSig = if null inputTypes
                then "void"
                else intercalate ", " $ (\(t',i') -> toCString c t' ++ " " ++ toCString c i') <$> zip inputTypes is
            prototype = makeHeaderLines [ returnString ++ " " ++ toCString c i ++ "(" ++ paramsPrototypes ++ ");" ]
            paramsPrototypes = if null inputTypes
                then "void"
                else intercalate ", " (toCString c <$> inputTypes)
            body = foldr1 (<>) $ toC (moreIndent c) <$> bs
            returnString = toCString c returnType
            inputTypes = init $ getTypeList t
            returnType = last $ getTypeList t

instance ToCString EmperorType where
    toCString _ IntP = "int"
    toCString _ CharP = "char"
    toCString _ BoolP = "int"
    toCString _ RealP = "double"
    toCString _ (ESet _) = "emperorList_t*"
    toCString _ (EList _) = "emperorList_t*"
    toCString c (ETuple ts) = intercalate "__" $ "eTuple_t" : (toCString c <$> ts)
    toCString _ (ERecord _) = error "Records are not yet supported... working on it!"
    toCString _ (EFunction _ _ _) = error "Functions are not yet supported properly :/ (Working on it)"
    toCString _ Any = "void*"
    toCString _ Unit = "void"

instance ToC BodyBlock where
    -- TODO: make blocks actually work!
    toC c (Line l _) = toC c l
    toC c (IfElse e _ _ _) = makeBodyLines ([makeIndent c ++ "if (" ++ toCString (moreIndent c) e ++ ")", makeIndent c ++ "{"] ++ ["}", "else", "{"] ++ ["}"])
    toC c (While e _ _) = makeBodyLines ([makeIndent c ++ "while (" ++ toCString (moreIndent c) e ++ ")", makeIndent c ++ "{"] ++ [makeIndent c ++ "}"])
    toC _ (For _ _ _ _) = makeBodyLines ["for..."]
    toC c (Repeat e _ _) = makeBodyLines ([makeIndent c ++ "for (int i = 0; i < " ++ toCString (moreIndent c) e ++ "; i++)", makeIndent c ++ "{"] ++ [makeIndent c ++ "}"])
    toC _ (With _ _ _) = makeBodyLines ["with"]
    toC _ (Switch _ _ _) = makeBodyLines ["switch"]

instance ToC BodyLine where
    toC c (AssignmentC x) = generatePosLines makeBodyLines c x <> wrapLastBodyLine (toC c x) (\x' -> makeIndent c ++ x' ++ ";")
    toC c (QueueC x) = generatePosLines makeBodyLines c x <> wrapLastBodyLine (toC c x) (\x' -> makeIndent c ++ x' ++ ";")
    toC c (CallC x) = generatePosLines makeBodyLines c x <> wrapLastBodyLine (toC c x) (\x' -> makeIndent c ++ x' ++ ";")
    toC c (Return Nothing p) = generatePosLines makeBodyLines c (Return Nothing p) <> makeBodyLines [makeIndent c ++ "return" ++ ";"]
    toC c (Return (Just v) p) = generatePosLines makeBodyLines c (Return (Just v) p) <> makeBodyLines [makeIndent c ++ "return " ++ toCString c v ++ ";"]

generatePosLines :: GetPos a => ([String] -> GenerationResult) ->  GenerationContext -> a -> GenerationResult
generatePosLines f c x = let s = generatePos c x in if null s
    then mempty
    else f [s]

instance ToC Assignment where
    toC c (Assignment Nothing i e _) = makeBodyLines [toCString c i ++ " = " ++ toCString c e]
    toC c (Assignment (Just t) i e _) = makeBodyLines [toCString c t ++ " " ++ toCString c i ++ " = " ++ toCString c e]

instance ToC Queue where
    toC c (Queue Nothing i e _) = makeBodyLines [toCString c i ++ " = " ++ toCString c e]
    toC c (Queue (Just t) i e _) = makeBodyLines [toCString c t ++ " " ++ toCString c i ++ " = " ++ toCString c e]

instance ToCString Expr where
    toCString c (Value v _) = toCString c v
    toCString c (Neg e _) = unaryOp c "-" e
    toCString c (Add e1 e2 _) = binaryOp c "+" e1 e2
    toCString c (Subtract e1 e2 _) = binaryOp c "-" e1 e2
    toCString c (Multiply e1 e2 _) = binaryOp c "*" e1 e2
    toCString c (Divide e1 e2 _) = binaryOp c "/" e1 e2
    toCString c (Modulo e1 e2 _) = binaryOp c "%" e1 e2
    toCString c (Less e1 e2 _) = binaryOp c "<" e1 e2
    toCString c (LessOrEqual e1 e2 _) = binaryOp c "<=" e1 e2
    toCString c (Greater e1 e2 _) = binaryOp c ">" e1 e2
    toCString c (GreaterOrEqual e1 e2 _) = binaryOp c ">=" e1 e2
    toCString c (Equal e1 e2 _) = binaryOp c "==" e1 e2
    toCString c (NotEqual e1 e2 _) = binaryOp c "!=" e1 e2
    toCString c (Not e _) = unaryOp c "!" e
    toCString c (AndStrict e1 e2 _) = binaryOp c "&" e1 e2
    toCString c (AndLazy e1 e2 _) = binaryOp c "&&" e1 e2
    toCString c (OrStrict e1 e2 _) = binaryOp c "|" e1 e2
    toCString c (OrLazy e1 e2 _) = binaryOp c "||" e1 e2
    toCString c (Implies e1 e2 p) = binaryOp c "||" (Not e1 p) e2
    toCString c (Xor e1 e2 _) = binaryOp c "^" e1 e2
    toCString c (ShiftLeft e1 e2 _) = binaryOp c "<<" e1 e2
    toCString c (ShiftRight e1 e2 _) = binaryOp c ">>" e1 e2
    toCString c (ShiftRightSameSign e1 e2 _) = binaryOp c ">>>" e1 e2
    toCString _ (Set _ _) = error "Sets are not supported yet..."
    toCString _ (Tuple _ _) = error "Tuples are not supported yet..."
    toCString _ (List _ _) = error "Lists are not supported yet..."

unaryOp :: GenerationContext -> String -> Expr -> String
unaryOp c o e = o ++ toCString c e

binaryOp :: GenerationContext -> String -> Expr -> Expr -> String
binaryOp c o e1 e2 = '(' : toCString c e1 ++ ' ' : o ++ " " ++ toCString c e2 ++ ")"

instance ToCString Value where
    toCString _ (Integer i _) = show i
    toCString _ (Real r _) = show r
    toCString _ (Char c _) = show c
    toCString _ (StringV s _) = show s
    toCString c (IdentV i _) = toCString c i
    toCString _ (Bool b _) = if b then "1" else "0"
    toCString c (CallV c' _) = toCString c c'

instance ToC Call where
    toC c c' = makeBodyLines [toCString c c']

instance ToCString Call where
    toCString c (Call _ i es _) = toCString c i ++ "(" ++ intercalate ", " (toCString c <$> es) ++ ")"

instance ToCString Ident where
    toCString _ (Ident s _) = s

instance ToCString String where
    toCString _ s = s
