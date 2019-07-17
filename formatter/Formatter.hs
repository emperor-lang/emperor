module Formatter where

import AST

type FormatContext = Int

defaultFormatContext :: FormatContext
defaultFormatContext = 0

class Show a => Format a where
    format :: FormatContext -> a -> String
    formatFresh :: a -> String
    formatFresh = format defaultFormatContext

instance Format AST where
    format ctx (AST a) = unlines $ format ctx <$> a

instance Format BodyLine where
    format ctx (BodyLine _ i v) = indent ctx ++ format ctx i ++ " = " ++ format ctx v

instance Format Value where
    format _ (Integer i)    = show i
    format _ (Real r)       = show r
    format _ (Char c)       = show c
    format _ (IdentV s)     = s
    format _ (Bool b)       = show b

instance Format Ident where
    format _ (Ident i) = i

instance Format Tabs where
    format ctx _ = indent ctx

indent :: FormatContext -> String
indent ctx = replicate ctx '\t'
