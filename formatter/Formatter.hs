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
module Formatter where

import AST
import Data.List

-- | The information required to format code in a given context
type FormatContext = Int

-- | Create a new context for formatting (e.g. no indentation)
defaultFormatContext :: FormatContext
defaultFormatContext = 0

-- | Describes items which may be formatted
class Format a where
    -- | Create a string which represents the object in the current context
    format :: FormatContext -> a -> String

    -- | Format from a blank context
    formatFresh :: a -> String
    formatFresh = format defaultFormatContext

-- | Lists are formattable by placing commas between elements
instance Format a => Format [a] where
    format ctx l = intercalate ", " $ format ctx <$> l

-- | The AST may be formatted by interspercing new-line characters
instance Format AST where
    format ctx (AST (Just d) a) = (format ctx d) ++ format ctx (AST Nothing a)
    format ctx (AST Nothing a) = (unlines $ format ctx <$> a)

-- | Doclines may be formatted as they appear
instance Format DocLines where
    format ctx (DocLines ds) = makeLines $ [indent ctx ++ "/*"] ++ (format ctx <$> ds) ++ [indent ctx ++ "*/"] 

-- | Doclines are formatted as comments
instance Format DocLine where
    format ctx (DocLine d) = indent ctx ++ " * " ++ d

-- | Body block may be formatted with included code indented one layer further
instance Format BodyBlock where
    format ctx (Line l)         = format ctx l
    format ctx (IfElse c b1 b2) = makeLines $ [ indent ctx ++ "if " ++ format (ctx + 1) c ++ ":"] ++ (format (ctx + 1) <$> b1) ++ [indent ctx ++ "#"] ++ [indent ctx ++ "else"] ++ (format (ctx + 1) <$> b2) 
    format ctx (While c b)      = makeLines $ [ indent ctx ++ "while " ++ format (ctx + 1) c ++ ":"] ++ (format (ctx + 1) <$> b) ++ [indent ctx ++ "#"]
    format ctx (For i e b)      = makeLines $ [ indent ctx ++ "for " ++ format (ctx + 1) i ++ " <- " ++ format (ctx + 1) e ++ ":"] ++ (format (ctx + 1) <$> b) ++ [indent ctx ++ "#"]
    format ctx (Repeat c b)     = makeLines $ [ indent ctx ++ "repeat " ++ format (ctx + 1) c ++ ":"] ++ (format (ctx + 1) <$> b) ++ [indent ctx ++ "#"]
    format ctx (With a b)       = makeLines $ [ indent ctx ++ "with " ++ format (ctx + 1) a ++ ":"] ++ (format (ctx + 1) <$> b) ++ [indent ctx ++ "#"]
    format ctx (Switch c s)     = makeLines $ [ indent ctx ++ "switch " ++ format (ctx + 1) c ++ ":"] ++ (format (ctx + 1) <$> s) ++ [indent ctx ++ "#"]

-- | Switch-cases may be formatted with their case contents
instance Format SwitchCase where
    format ctx (SwitchCase e b) = indent ctx ++ format (ctx + 1) e ++ " -> " ++ format (ctx + 1) b

-- | Body-lines may be formatted by indenting and formatting their contents
instance Format BodyLine where
    format ctx (BodyLine _ b) = indent ctx ++ format ctx b

-- | Body-line contents may beformatted
instance Format BodyLineContent where
    format ctx (AssignmentC a) = format ctx a
    format ctx (QueueC q) = format ctx q
    format ctx (ImpureCallC c) = format ctx c

-- | Assignments may be formatted using their left and right sides
instance Format Assignment where
    format ctx (Assignment i e) = format ctx i ++ " = " ++ format (ctx + 1) e
    
-- | Queue statements may be formatted using their left and right sides
instance Format Queue where
    format ctx (Queue i e) = format ctx i ++ " <- " ++ format (ctx + 1) e

-- | Expressions may be formatted according to their context
instance Format Expr where
    format ctx (Value v)                                        = format ctx v
    format ctx (Neg e)                                          = '-' : format ctx e
    format ctx (Not e)                                          = '!' : format ctx e                         
    format ctx (Add e1 e2)                                      = formatBinOp ctx "+" e1 e2
    format ctx (Subtract e1 e2)                                 = formatBinOp ctx "-" e1 e2
    format ctx (Multiply e1 e2)                                 = formatBinOp ctx "*" e1 e2
    format ctx (Divide e1 e2)                                   = formatBinOp ctx "/" e1 e2
    format ctx (Modulo e1 e2)                                   = formatBinOp ctx "%" e1 e2
    format ctx (Less e1 e2)                                     = formatBinOp ctx "<" e1 e2
    format ctx (LessOrEqual e1 e2)                              = formatBinOp ctx "<=" e1 e2
    format ctx (Greater e1 e2)                                  = formatBinOp ctx ">" e1 e2
    format ctx (GreaterOrEqual e1 e2)                           = formatBinOp ctx ">=" e1 e2
    format ctx (Equal e1 e2)                                    = formatBinOp ctx "==" e1 e2
    format ctx (NotEqual e1 e2)                                 = formatBinOp ctx "!=" e1 e2
    format ctx (AndStrict e1 e2)                                = formatBinOp ctx "&" e1 e2
    format ctx (AndLazy e1 e2)                                  = formatBinOp ctx "&&" e1 e2
    format ctx (OrStrict e1 e2)                                 = formatBinOp ctx "|" e1 e2
    format ctx (OrLazy e1 e2)                                   = formatBinOp ctx "||" e1 e2
    format ctx (Implies e1 e2)                                  = formatBinOp ctx "=>" e1 e2
    format ctx (Xor e1 e2)                                      = formatBinOp ctx "^" e1 e2
    format ctx (ShiftLeft e1 e2)                                = formatBinOp ctx "<<" e1 e2
    format ctx (ShiftRight e1 e2)                               = formatBinOp ctx ">>" e1 e2
    format ctx (ShiftRightSameSign e1 e2)                       = formatBinOp ctx ">>>" e1 e2
    format ctx (Set l)                                          = "(" ++ format ctx l ++ "}"
    format ctx (Tuple l)                                        = "(" ++ format ctx l ++ ")"
    format ctx (List l)                                         = "[" ++ format ctx l ++ "]"
    format ctx (PureCallExpr c)                                 = format ctx c
    format ctx (ImpureCallExpr c)                               = format ctx c

-- | Binary operators are formatted with the operator symbol surrounded by spaces and the formatted left and right expressions
formatBinOp :: Format a => Format b => FormatContext -> String -> a -> b -> String
formatBinOp ctx op e1 e2 = format ctx e1 ++ " " ++ op ++ " " ++ format ctx e2

-- | Pure calls are formatted as their identifier and arguments
instance Format PureCall where
    format ctx (PureCall i e) = format ctx i ++ "(" ++ format (ctx + 1) e ++ ")"
    
-- | Impure calls are formatted as their identifier and arguments
instance Format ImpureCall where
    format ctx (ImpureCall i e) = "@" ++ format ctx (PureCall i e)

-- | Values are formatted according to their type
instance Format Value where
    format _ (Integer i)    = show i
    format _ (Real r)       = show r
    format _ (Char c)       = show c
    format _ (IdentV s)     = s
    format _ (Bool b)       = if b then "true" else "false"

-- | Identifiers are formatted as their name
instance Format Ident where
    format _ (Ident i) = i

-- | Tabs are formatted as tabs
instance Format Tabs where
    format ctx _ = indent ctx

-- | Create the appropriate amount of indentation for the current context
indent :: FormatContext -> String
indent ctx = replicate ctx '\t'

makeLines :: [String] -> String
makeLines xs = concat $ makeLines' xs
    where
        makeLines' :: [String] -> [String]
        makeLines' [] = []
        makeLines' (x:[]) = [x]
        makeLines' (x:xs') = x : "\n" : makeLines' xs'