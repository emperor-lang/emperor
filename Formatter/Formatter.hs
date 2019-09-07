{-|
Module      : Formatter
Description : Code formatter
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

This file defines the standard format for all Emperor programs.
-}
module Formatter.Formatter
    ( format
    , formatFresh
    , Format
    , FormatContext
    ) where

import Data.List (intercalate, sort)
import Data.Map ((!), keys)
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
import Types.Results (EmperorType(..), Purity(..))

-- | The information required to format code in a given context
type FormatContext = Int

-- | Create a new context for formatting (e.g. no indentation)
defaultFormatContext :: FormatContext
defaultFormatContext = 0

-- | Describes items which may be formatted
class Format a where
    format :: FormatContext -> a -> String
    -- ^ Create a string which represents the object in the current context
    formatFresh :: a -> String
    -- ^ Format from a blank context
    formatFresh = format defaultFormatContext

-- | Lists are formattable by placing commas between elements
instance Format a => Format [a] where
    format ctx l = intercalate ", " $ format ctx <$> l

-- | The AST may be formatted by intercalating new-line characters
instance Format AST where
    format ctx (AST m is a) = unlines (format ctx m : "" : sort (format ctx <$> is) ++ "" : (format ctx <$> a))

-- | Imports may be formatted as a sorted list of their elements
instance Format Import where
    format ctx (Import l Nothing _) = "import " ++ format ctx l
    format ctx (Import l (Just is) _) =
        "import " ++ format ctx l ++ " (" ++ intercalate ", " (format ctx <$> sort is) ++ ")"

-- | Import locations may be formatted by their type
instance Format ImportLocation where
    format ctx (ImportLocation Global i _) = "<" ++ format ctx i ++ ">"
    format ctx (ImportLocation Local i _) = "\"" ++ format ctx i ++ "\""

-- | Module headers may be formatted with their docs and name
instance Format ModuleHeader where
    format ctx (Module i Nothing _) = "module " ++ format ctx i
    format ctx (Module i (Just is) _) = "module " ++ format ctx i ++ '(' : format ctx is ++ ")"

-- | Module items may be formatted by their contents
instance Format ModuleItem where
    format ctx (Component i c bs _) =
        "component " ++
        format ctx i ++ typeComparisonString ++ ":\n" ++ unlines (format (ctx + 1) <$> bs) ++ indent ctx ++ "#\n"
      where
        typeComparisonString =
            let s = formatTypeComparisons ctx c
             in if null s
                    then ""
                    else ' ' : s
    format ctx (TypeClass i c bs _) =
        "class " ++
        format ctx i ++ typeComparisonString ++ ":\n" ++ unlines (format (ctx + 1) <$> bs) ++ indent ctx ++ "#\n"
      where
        typeComparisonString =
            let s = formatTypeComparisons ctx c
             in if null s
                    then ""
                    else ' ' : s
    format ctx (FunctionItem f _) = format ctx f ++ "\n"

formatTypeComparisons :: FormatContext -> Maybe [TypeComparison] -> String
formatTypeComparisons _ Nothing = ""
formatTypeComparisons ctx (Just cs) = unwords $ format ctx <$> cs

instance Format FunctionDef where
    format ctx (FunctionDef (FunctionTypeDef i t p) is bs _) =
        indent ctx ++
        format ctx (FunctionTypeDef i t p) ++
        "\n" ++ indent ctx ++ format ctx i ++ params ++ ":\n" ++ unlines (format (ctx + 1) <$> bs) ++ indent ctx ++ "#"
      where
        params =
            if null paramIdentifierString
                then ""
                else ' ' : paramIdentifierString
        paramIdentifierString = format ctx is

instance Format FunctionTypeDef where
    format ctx (FunctionTypeDef i t _) = format ctx i ++ " :: " ++ format ctx t

-- | Type comparisons are formatted with whitespace
instance Format TypeComparison where
    format ctx (IsSubType i _) = "<: " ++ format ctx i
    format ctx (IsSubTypeWithImplementor i i' _) = "<:" ++ format ctx i ++ " <~ " ++ format ctx i'

-- | Types can be formatted as they would appear
instance Format EmperorType where
    format _ IntP = "int"
    format _ CharP = "char"
    format _ BoolP = "bool"
    format _ RealP = "real"
    format ctx (ESet t) = "{" ++ format ctx t ++ "}"
    format ctx (EList t) = "[" ++ format ctx t ++ "]"
    format ctx (ETuple ts) = intercalate "*" (format ctx <$> ts)
    format ctx (ERecord m) = " { " ++ formattedMap ++ " }"
      where
        formattedMap :: String
        formattedMap = intercalate ", " [k ++ " : " ++ format ctx (m ! k) | k <- keys m]
    format ctx (EFunction p (EFunction p' t1 t1') t2) =
        format ctx p ++ "(" ++ format ctx (EFunction p' t1 t1') ++ ") -> " ++ format ctx t2
    format ctx (EFunction p t1 t2) = format ctx p ++ format ctx t1 ++ " -> " ++ format ctx t2
    format _ Any = "Any"
    format _ Unit = "Unit"

-- | Body block may be formatted with included code indented one layer further
instance Format BodyBlock where
    format ctx (Line l _) = format ctx l
    format ctx (IfElse c b1 b2 _) =
        unlines $
        [indent ctx ++ "if " ++ format (ctx + 1) c ++ ":"] ++
        (format (ctx + 1) <$> b1) ++ [indent ctx ++ "#else:"] ++ (format (ctx + 1) <$> b2) ++ [indent ctx ++ "#"]
    format ctx (While c b _) =
        unlines $ [indent ctx ++ "while " ++ format (ctx + 1) c ++ ":"] ++ (format (ctx + 1) <$> b) ++ [indent ctx ++ "#"]
    format ctx (For i e b _) =
        unlines $
        [indent ctx ++ "for " ++ format (ctx + 1) i ++ " <- " ++ format (ctx + 1) e ++ ":"] ++
        (format (ctx + 1) <$> b) ++ [indent ctx ++ "#"]
    format ctx (Repeat c b _) =
        unlines $ [indent ctx ++ "repeat " ++ format (ctx + 1) c ++ ":"] ++ (format (ctx + 1) <$> b) ++ [indent ctx ++ "#"]
    format ctx (With t i e bs p) =
        unlines $ [indent ctx ++ "with " ++ format (ctx + 1) (Assignment (Just t) i e p) ++ ":"] ++ (format (ctx + 1) <$> bs) ++ [indent ctx ++ "#"]
    format ctx (Switch c s _) =
        unlines $ [indent ctx ++ "switch " ++ format (ctx + 1) c ++ ":"] ++ (format (ctx + 1) <$> s) ++ [indent ctx ++ "#"]

-- | Switch-cases may be formatted with their case contents
instance Format SwitchCase where
    format ctx (SwitchCase e b _) = indent ctx ++ format (ctx + 1) e ++ " -> " ++ format (ctx + 1) b

-- | Body-lines may be formatted by their structure
instance Format BodyLine where
    format ctx (AssignmentC a) = indent ctx ++ format ctx a
    format ctx (QueueC q) = indent ctx ++ format ctx q
    format ctx (CallC c) = indent ctx ++ format ctx c
    format ctx (Return e _) = indent ctx ++ "return " ++ format ctx e

-- | Assignments may be formatted using their left and right sides
instance Format Assignment where
    format ctx (Assignment t i e _) = unwords [format ctx t, format ctx i, "=", format (ctx + 1) e]

-- | Queue statements may be formatted using their left and right sides
instance Format Queue where
    format ctx (Queue t i e _) = unwords [format ctx t, format ctx i, "=", format (ctx + 1) e]

-- | Expressions may be formatted according to their context
instance Format Expr where
    format ctx (Value v _) = format ctx v
    format ctx (Neg e _) = '-' : format ctx e
    format ctx (Not e _) = '!' : format ctx e
    format ctx (Add e1 e2 _) = formatBinOp ctx "+" e1 e2
    format ctx (Subtract e1 e2 _) = formatBinOp ctx "-" e1 e2
    format ctx (Multiply e1 e2 _) = formatBinOp ctx "*" e1 e2
    format ctx (Divide e1 e2 _) = formatBinOp ctx "/" e1 e2
    format ctx (Modulo e1 e2 _) = formatBinOp ctx "%" e1 e2
    format ctx (Less e1 e2 _) = formatBinOp ctx "<" e1 e2
    format ctx (LessOrEqual e1 e2 _) = formatBinOp ctx "<=" e1 e2
    format ctx (Greater e1 e2 _) = formatBinOp ctx ">" e1 e2
    format ctx (GreaterOrEqual e1 e2 _) = formatBinOp ctx ">=" e1 e2
    format ctx (Equal e1 e2 _) = formatBinOp ctx "==" e1 e2
    format ctx (NotEqual e1 e2 _) = formatBinOp ctx "!=" e1 e2
    format ctx (AndStrict e1 e2 _) = formatBinOp ctx "&" e1 e2
    format ctx (AndLazy e1 e2 _) = formatBinOp ctx "&&" e1 e2
    format ctx (OrStrict e1 e2 _) = formatBinOp ctx "|" e1 e2
    format ctx (OrLazy e1 e2 _) = formatBinOp ctx "||" e1 e2
    format ctx (Implies e1 e2 _) = formatBinOp ctx "=>" e1 e2
    format ctx (Xor e1 e2 _) = formatBinOp ctx "^" e1 e2
    format ctx (ShiftLeft e1 e2 _) = formatBinOp ctx "<<" e1 e2
    format ctx (ShiftRight e1 e2 _) = formatBinOp ctx ">>" e1 e2
    format ctx (ShiftRightSameSign e1 e2 _) = formatBinOp ctx ">>>" e1 e2
    format ctx (Set l _) = "(" ++ format ctx l ++ "}"
    format ctx (Tuple l _) = "(" ++ format ctx l ++ ")"
    format ctx (List l _) = "[" ++ format ctx l ++ "]"

-- | Binary operators are formatted with the operator symbol surrounded by spaces and the formatted left and right expressions
formatBinOp ::
       Format a
    => Format b =>
           FormatContext -> String -> a -> b -> String
formatBinOp ctx op e1 e2 = format ctx e1 ++ " " ++ op ++ " " ++ format ctx e2

-- | Values are formatted according to their type
instance Format Value where
    format _ (Integer i _) = show i
    format _ (Real r _) = show r
    format _ (Char c _) = show c
    format ctx (IdentV i _) = format ctx i
    format _ (StringV s _) = show s
    format _ (Bool b _) =
        if b
            then "true"
            else "false"
    format ctx (CallV c _) = format ctx c

-- | Calls are formatted by their contents
instance Format Call where
    format ctx (Call p i es _) = format ctx p ++ format ctx i ++ '(' : format ctx es ++ ")"

-- | Impure functions have an "\@" in from of them, pure ones have nothing
instance Format Purity where
    format _ Pure = ""
    format _ Impure = "@"

-- | Identifiers are formatted as their name
instance Format Ident where
    format _ (Ident i _) = i

-- | Maybe formattables may be formatted as an empty string or their contents
instance Format a => Format (Maybe a) where
    format _ Nothing = ""
    format ctx (Just x) = format ctx x

-- | Create the appropriate amount of indentation for the current context
indent :: FormatContext -> String
indent ctx = replicate ctx '\t'
