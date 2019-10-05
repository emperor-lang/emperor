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
module CodeGenerator.ToC
    ( ToC
    , ToCString
    , toC
    , toCString
    ) where

import           CodeGenerator.Context  (GenerationContext, destFile, exposedIdents, makeIndent, moduleName, moreIndent,
                                         nativeCompile, sourceFile)
import           CodeGenerator.Position (GetPos, generatePos)
import           CodeGenerator.Results  (GenerationResult, makeBodyLines, makeHeaderAndBodyLines, makeHeaderLines,
                                         wrapLastBodyLine)
import           Data.Char              (toUpper)
import           Data.List              (intercalate, intersperse)
import           Data.Monoid            ((<>))
import           Parser.AST             (AST (..), Assignment (..), BodyBlock (..), BodyLine (..), Call (..), Expr (..),
                                         FunctionDef (..), FunctionTypeDef (..), Ident (..), Import (..),
                                         ImportLocation (..), ImportType (..), ModuleHeader (..), ModuleItem (..),
                                         Queue (..), Value (..), stringRep)
import           Parser.EmperorLexer    (AlexPosn (..))
import           Types.Results          (EmperorType (..), Purity (Impure), getTypeList)


class ToC a where
    toC :: GenerationContext -> a -> GenerationResult

class ToCString a where
    toCString :: GenerationContext -> a -> String

instance ToC AST where
    toC c (AST (Module is mis p) is' ms) = makeHeaderLines ["#ifndef " ++ includeGuard, "#define " ++ includeGuard, ""] <>
        toC c (Module is mis p) <>
        foldr (<>) mempty (toC c' <$> is') <>
        makeHeaderLines [""] <>
        foldr (<>) mempty (toC c' <$> ms) <> makeHeaderLines ["", "#endif /* " ++ includeGuard ++ " */"]
            where
                includeGuard = "__" ++ (toUpper <$> ((sanitise .  sourceFile) c)) ++ "_H_"

                c' = c { exposedIdents = mis, moduleName = is }

                sanitise :: String -> String
                sanitise s = if head s `elem` ['.', '/']
                        then sanitise $ tail s
                        else replace <$> s
                    where
                        replace :: Char -> Char
                        replace '.' = '_'
                        replace '/' = '$'
                        replace x   = x

instance ToC ModuleHeader where
    toC c (Module i mis p) = generatePosLines makeHeaderAndBodyLines c (Module i mis p) <> headerInclude <> makeHeaderAndBodyLines ["// This is module " ++ show (toCString c i) ++ " generated from " ++ (show . sourceFile) c ++ " by emperor"] <> makeHeaderLines (dependencyPragma ++ ["#include <base.h>"])
        where
            headerInclude = makeBodyLines $ if destFile c /= "stdout" && (not . nativeCompile) c then ["#include \"" ++ destFile c ++ ".h\""] else []
            dependencyPragma =  if sourceFile c /= "stdin" then ["#pragma GCC dependency " ++ (show . sourceFile) c, ""] else []

instance ToC Import where
    toC c (Import l _ _) = toC c l

instance ToC ImportLocation where
    toC c (ImportLocation Global i p) =
        generatePosLines makeHeaderLines c (ImportLocation Global i p) <>
        makeHeaderLines ["#include <" ++ toCString c i ++ ".h>"]
    toC c (ImportLocation Local i p) =
        generatePosLines makeHeaderLines c (ImportLocation Local i p) <>
        makeHeaderLines ["#include \"" ++ toCString c i ++ ".h\""]

instance ToC ModuleItem where
    toC _ Component {} =
        error $
        "Components have not been implemented yet, and this should have been caught at the type-checking stage??"
    toC _ TypeClass {} = mempty
    toC c (FunctionItem f p) =
        makeBodyLines [""] <> generatePosLines makeHeaderAndBodyLines c (FunctionItem f p) <> toC c f

instance ToC FunctionDef where
    toC c (FunctionDef (FunctionTypeDef i t _) is bs p) =
        optionalEntryPoint <>
        prototype <>
        makeBodyLines [returnString ++ " " ++ functionName ++ "(" ++ paramSig ++ ")", "{"] <>
        body <> makeBodyLines ["}"]
      where
        paramSig =
            if null inputTypeMap
                then "void"
                else intercalate ", " $ (\(i', t') -> toCString c t' ++ " " ++ toCString c i') <$> inputTypeMap
        prototype = makeHeaderLines [staticString ++ returnString ++ " " ++ functionName ++ "(" ++ paramsPrototypes ++ ");"]
        functionName = (stringRep . moduleName) c ++ '_' : toCString c i
        staticString = case exposedIdents c of
            Nothing  -> ""
            Just eis -> if i `elem` eis then "" else "static "
        paramsPrototypes =
            if null inputTypeMap
                then "void"
                else intercalate ", " (toCString c <$> ((\(_, x) -> x) <$> inputTypeMap))
        bs' = if i == Ident "main" p then
                (Line (CallC (Call Impure (Ident "base_initEmperor" p) [] p)) p) : bs
            else
                bs
        body = foldr (<>) mempty $ toC (moreIndent c) <$> bs'
        returnString = toCString c returnType
        inputTypeMap = filter (not . isUnit) $ zip is (init $ getTypeList t)
        isUnit (_, Unit) = True
        isUnit _         = False
        returnType = last $ getTypeList t

        optionalEntryPoint = case i of
            Ident "main" _ -> if (stringRep . moduleName) c == "main" then
                    let mainPrototype = makeHeaderLines [ "int main(int, char**);" ] in
                    let mainCallBody = [ makeIndent (moreIndent c) ++ "return main_main().intV;" ] in
                    let mainBody = makeBodyLines $ [ "int main(int UNUSED(argc), char** UNUSED(argv))", "{" ] ++ mainCallBody ++ [ "}", "" ] in
                    mainPrototype <> mainBody
                else
                    mempty
            _ -> mempty

instance ToCString EmperorType where
    toCString _ _ = "base_Any_t"

instance ToC BodyBlock where
    toC c (Line l _) = toC c l
    toC c (IfElse e bs1 bs2 p) =
        generatePosLines makeBodyLines c (IfElse e bs1 bs2 p) <>
        makeBodyLines [makeIndent c ++ "if (" ++ toCString (moreIndent c) e ++ ")", makeIndent c ++ "{"] <>
        (foldl (<>) mempty $ toC (moreIndent c) <$> bs1) <>
        makeBodyLines [makeIndent c ++ "}", makeIndent c ++ "else", makeIndent c ++ "{"] <>
        (foldl (<>) mempty $ toC (moreIndent c) <$> bs2) <> makeBodyLines [makeIndent c ++ "}"]
    toC c (While e bs p) =
        generatePosLines makeBodyLines c (While e bs p) <>
        makeBodyLines [makeIndent c ++ "while (" ++ toCString (moreIndent c) e ++ ")", makeIndent c ++ "{"] <>
        (foldl (<>) mempty $ toC (moreIndent c) <$> bs) <> makeBodyLines [makeIndent c ++ "}"]
    toC c (For i e bs p) = generatePosLines makeBodyLines c (For i e bs p) <>
        makeBodyLines [
            makeIndent c ++ "base_Any_t " ++ forListVar ++ " = " ++ "0;",
            makeIndent c ++ "for (base_EmperorListNode_t* " ++ forListNodeVar ++ " = ((base_EmperorList_t*)" ++ forListNodeVar ++ ".voidV)->first.voidV; " ++ forListNodeVar ++ ".voidV != NULL; " ++ forListNodeVar ++ " = " ++ forListVar ++ "->next.voidV)",
            makeIndent c ++ "{",
            makeIndent (moreIndent c) ++ "base_Any_t " ++ toCString c i ++ " = " ++ forListNodeVar ++ "->value;"
        ] <> (foldl (<>) mempty $ toC (moreIndent c) <$> bs) <> makeBodyLines [makeIndent c ++ "}"]
        where
            forListVar = "forListVar__" ++ toCString c p
            forListNodeVar = "forListNodeVar__" ++ toCString c p
    toC c (Repeat e bs _) =
        makeBodyLines [makeIndent c ++ "for (int i = 0; i < " ++ toCString (moreIndent c) e ++ "; i++)", makeIndent c ++ "{"] <> (foldr (<>) mempty $ toC (moreIndent c) <$> bs) <> makeBodyLines [makeIndent c ++ "}"]
    toC _ (With _ _ _ _ _) = error "With has not been implemented yet" -- Use if (1) for scoping
    toC _ (Switch _ _ _) = error "Switches have not been implemented yet"

instance ToC BodyLine where
    toC c (AssignmentC x) =
        generatePosLines makeBodyLines c x <> wrapLastBodyLine (toC c x) (\x' -> makeIndent c ++ x' ++ ";")
    toC c (QueueC x) =
        generatePosLines makeBodyLines c x <> wrapLastBodyLine (toC c x) (\x' -> makeIndent c ++ x' ++ ";")
    toC c (CallC x) =
        generatePosLines makeBodyLines c x <> wrapLastBodyLine (toC c x) (\x' -> makeIndent c ++ x' ++ ";")
    toC c (Return Nothing p) =
        generatePosLines makeBodyLines c (Return Nothing p) <> makeBodyLines [makeIndent c ++ "return" ++ ";"]
    toC c (Return (Just v) p) =
        generatePosLines makeBodyLines c (Return (Just v) p) <>
        makeBodyLines [makeIndent c ++ "return " ++ toCString c v ++ ";"]

generatePosLines :: GetPos a => ([String] -> GenerationResult) -> GenerationContext -> a -> GenerationResult
generatePosLines f c x =
    let s = generatePos c x
     in if null s
            then mempty
            else f [s]

instance ToC Assignment where
    toC c (Assignment Nothing i e _) = makeBodyLines [toCString c i ++ " = " ++ toCString c e]
    toC c (Assignment (Just t) i e _) = makeBodyLines [toCString c t ++ " " ++ toCString c i ++ " = " ++ toCString c e]

instance ToC Queue where
    toC c (Queue Nothing i e _)  = makeBodyLines [toCString c i ++ " = " ++ toCString c e]
    toC c (Queue (Just t) i e _) = makeBodyLines ["base_Any_t " ++ toCString c i ++ " = (base_Any_t){ ." ++ typeModifier ++ " = " ++ toCString c e ++ " }." ++ typeModifier]
        where
            typeModifier = case t of
                IntP  -> "intV"
                CharP -> "charV"
                BoolP -> "intV"
                RealP -> "doubleV"
                _     -> "voidV"

instance ToCString Expr where
    toCString c (Value v _)                  = toCString c v
    toCString c (Neg e _)                    = unaryOp c "-" e
    toCString c (Add e1 e2 _)                = binaryOp c "+" e1 e2
    toCString c (Subtract e1 e2 _)           = binaryOp c "-" e1 e2
    toCString c (Multiply e1 e2 _)           = binaryOp c "*" e1 e2
    toCString c (Divide e1 e2 _)             = binaryOp c "/" e1 e2
    toCString c (Modulo e1 e2 _)             = binaryOp c "%" e1 e2
    toCString c (Less e1 e2 _)               = binaryOp c "<" e1 e2
    toCString c (LessOrEqual e1 e2 _)        = binaryOp c "<=" e1 e2
    toCString c (Greater e1 e2 _)            = binaryOp c ">" e1 e2
    toCString c (GreaterOrEqual e1 e2 _)     = binaryOp c ">=" e1 e2
    toCString c (Equal e1 e2 _)              = binaryOp c "==" e1 e2
    toCString c (NotEqual e1 e2 _)           = binaryOp c "!=" e1 e2
    toCString c (Not e _)                    = unaryOp c "!" e
    toCString c (AndStrict e1 e2 _)          = binaryOp c "&" e1 e2
    toCString c (AndLazy e1 e2 _)            = binaryOp c "&&" e1 e2
    toCString c (OrStrict e1 e2 _)           = binaryOp c "|" e1 e2
    toCString c (OrLazy e1 e2 _)             = binaryOp c "||" e1 e2
    toCString c (Implies e1 e2 p)            = binaryOp c "||" (Not e1 p) e2
    toCString c (Xor e1 e2 _)                = binaryOp c "^" e1 e2
    toCString c (ShiftLeft e1 e2 _)          = binaryOp c "<<" e1 e2
    toCString c (ShiftRight e1 e2 _)         = binaryOp c ">>" e1 e2
    toCString c (ShiftRightSameSign e1 e2 _) = binaryOp c ">>>" e1 e2
    toCString _ (Set _ _)                    = error "Sets are not supported yet..."
    toCString _ (Tuple _ _)                  = error "Tuples are not supported yet..."
    toCString c (List es _)                  = "base_listFromArray((base_Any_t[]){ " ++ intercalate ", " (toCString c <$> es) ++ " }" ++ ", " ++ (show . length) es ++ ")"

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
    toCString _ (Bool b _) =
        if b
            then "1"
            else "0"
    toCString c (CallV c' _) = toCString c c'

instance ToC Call where
    toC c c' = makeBodyLines [toCString c c']

instance ToCString Call where
    toCString c (Call _ i es _) = toCString c i ++ "(" ++ intercalate ", " (toCString c <$> es) ++ ")"

instance ToCString Ident where
    toCString _ (Ident s _) = s

instance ToCString String where
    toCString _ s = s

instance ToCString AlexPosn where
    toCString _ (AlexPn a b c) = foldr1 (++) $ intersperse "_" $ show <$> [a,b,c]
