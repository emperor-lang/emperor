module Formatter where

import AST
import Data.List

type FormatContext = Int

defaultFormatContext :: FormatContext
defaultFormatContext = 0

class Format a where
    format :: FormatContext -> a -> String
    formatFresh :: a -> String
    formatFresh = format defaultFormatContext

instance Format a => Format [a] where
    format ctx l = intercalate ", " $ format ctx <$> l

instance Format AST where
    format ctx (AST a) = unlines $ format ctx <$> a 

instance Format BodyBlock where
    format ctx (Line l)         = format ctx l
    format ctx (IfElse c b1 b2) = unlines $ [ indent ctx ++ "if " ++ format (ctx + 1) c] ++ (format (ctx + 1) <$> b1) ++ [indent ctx ++ "else"] ++ (format (ctx + 1) <$> b2) 
    format ctx (While c b)      = unlines $ [ indent ctx ++ "while " ++ format (ctx + 1) c] ++ (format (ctx + 1) <$> b)
    format ctx (For i e b)      = unlines $ [ indent ctx ++ "for " ++ format (ctx + 1) i ++ " <- " ++ format (ctx + 1) e] ++ (format (ctx + 1) <$> b)
    format ctx (Repeat c b)     = unlines $ [ indent ctx ++ "repeat " ++ format (ctx + 1) c] ++ (format (ctx + 1) <$> b)
    format ctx (With a b)       = unlines $ [ indent ctx ++ "with " ++ format (ctx + 1) a] ++ (format (ctx + 1) <$> b)
    format ctx (Switch c s)     = unlines $ [ indent ctx ++ "switch " ++ format (ctx + 1) c] ++ (format (ctx + 1) <$> s)

instance Format SwitchCase where
    format ctx (SwitchCase e b) = indent ctx ++ format (ctx + 1) e ++ " -> " ++ format (ctx + 1) b

instance Format BodyLine where
    format ctx (BodyLine _ b) = indent ctx ++ format ctx b

instance Format BodyLineContent where
    format ctx (AssignmentC a) = format ctx a
    format ctx (QueueC q) = format ctx q
    format ctx (ImpureCallC c) = format ctx c

instance Format Assignment where
    format ctx (Assignment i e) = format ctx i ++ " = " ++ format (ctx + 1) e

instance Format Queue where
    format ctx (Queue i e) = format ctx i ++ " <- " ++ format (ctx + 1) e

instance Format Expr where
    format ctx (Value v)                                        = format ctx v
    format ctx (Neg e)                                          = '-' : format ctx e
    format ctx (Add e1 e2)                                      = format ctx e1 ++ "+" ++ format ctx e2
    format ctx (Subtract e1 e2)                                 = format ctx e1 ++ "-" ++ format ctx e2                    
    format ctx (Multiply e1 e2)                                 = format ctx e1 ++ "*" ++ format ctx e2                    
    format ctx (Divide e1 e2)                                   = format ctx e1 ++ "/" ++ format ctx e2                    
    format ctx (Less e1 e2)                                     = format ctx e1 ++ "<" ++ format ctx e2                    
    format ctx (LessOrEqual e1 e2)                              = format ctx e1 ++ "<=" ++ format ctx e2                   
    format ctx (Greater e1 e2)                                  = format ctx e1 ++ ">" ++ format ctx e2                    
    format ctx (GreaterOrEqual e1 e2)                           = format ctx e1 ++ ">=" ++ format ctx e2                   
    format ctx (Equal e1 e2)                                    = format ctx e1 ++ "==" ++ format ctx e2                   
    format ctx (NotEqual e1 e2)                                 = format ctx e1 ++ "!=" ++ format ctx e2                   
    format ctx (Not e)                                          = '!' : format ctx e                         
    format ctx (AndStrict e1 e2)                                = format ctx e1 ++ "&" ++ format ctx e2                    
    format ctx (AndLazy e1 e2)                                  = format ctx e1 ++ "&&" ++ format ctx e2                   
    format ctx (OrStrict e1 e2)                                 = format ctx e1 ++ "|" ++ format ctx e2                    
    format ctx (OrLazy e1 e2)                                   = format ctx e1 ++ "||" ++ format ctx e2                   
    format ctx (Implies e1 e2)                                  = format ctx e1 ++ "=>" ++ format ctx e2                   
    format ctx (Xor e1 e2)                                      = format ctx e1 ++ "^" ++ format ctx e2                    
    format ctx (ShiftLeft e1 e2)                                = format ctx e1 ++ "<<" ++ format ctx e2                   
    format ctx (ShiftRight e1 e2)                               = format ctx e1 ++ ">>" ++ format ctx e2                   
    format ctx (ShiftRightSameSign e1 e2)                       = format ctx e1 ++ ">>>" ++ format ctx e2                  
    format ctx (Set l)                                          = "(" ++ format ctx l ++ "}"
    format ctx (Tuple l)                                        = "(" ++ format ctx l ++ ")"
    format ctx (List l)                                         = "[" ++ format ctx l ++ "]"
    format ctx (PureCallExpr c)                                 = format ctx c
    format ctx (ImpureCallExpr c)                               = format ctx c

instance Format PureCall where
    format ctx (PureCall i e) = format ctx i ++ "(" ++ format (ctx + 1) e ++ ")"

instance Format ImpureCall where
    format ctx (ImpureCall i e) = "@" ++ format ctx (PureCall i e)

instance Format Value where
    format _ (Integer i)    = show i
    format _ (Real r)       = show r
    format _ (Char c)       = show c
    format _ (IdentV s)     = s
    format _ (Bool b)       = if b then "true" else "false"

instance Format Ident where
    format _ (Ident i) = i

instance Format Tabs where
    format ctx _ = indent ctx

indent :: FormatContext -> String
indent ctx = replicate ctx '\t'
