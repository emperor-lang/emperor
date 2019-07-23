{-|
Module      : AST
Description : Data-structures for the abstract syntax tree
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

This module defines the data-types used to form the abstract syntax tree of the
emperor language.
-}
module AST where

-- | Data type to represent the abstract syntax tree
data AST = AST (Maybe DocLines) [BodyBlock]
    deriving Show

-- | Represents a single construction in the body of a function. This may be a
-- further construct or just a single line
data BodyBlock = Line BodyLine                          -- ^ A single line of code
               | IfElse Expr [BodyBlock] [BodyBlock]    -- ^ An if-else block
               | While Expr [BodyBlock]                 -- ^ A while-loop
               | For Ident Expr [BodyBlock]             -- ^ A for-loop
               | Repeat Expr [BodyBlock]                -- ^ A repeat-loop
               | With Assignment [BodyBlock]            -- ^ A resource acquisition
               | Switch Expr [SwitchCase]               -- ^ A switch-case statement
    deriving Show

-- instance Functor SwitchCases where
--     fmap f (SwitchCases as) = SwitchCases (fmap f as)

-- | Data-struecture for the switch-case statement
data SwitchCase = SwitchCase Expr BodyBlock
    deriving Show
    
-- append :: SwitchCase -> SwitchCases -> SwitchCases
-- append a (SwitchCases as) = SwitchCases (a:as)

-- | Data-structure for a single body-line
data BodyLine = BodyLine Tabs BodyLineContent
    deriving Show

-- | Data-structure for the contents of a single line
data BodyLineContent = AssignmentC Assignment
                     | QueueC Queue
                     | ImpureCallC ImpureCall
    deriving Show

-- | Data-structure to represent an assignment statement
data Assignment = Assignment Ident Expr
    deriving Show

-- | Data-structure to represent an queue statement
data Queue = Queue Ident Expr
    deriving Show

-- | Data-structure to represent an expression
data Expr = Value Value
          | Neg Expr
          | Add Expr Expr
          | Subtract Expr Expr
          | Multiply Expr Expr
          | Divide Expr Expr
          | Modulo Expr Expr
          | Less Expr Expr
          | LessOrEqual Expr Expr
          | Greater Expr Expr
          | GreaterOrEqual Expr Expr
          | Equal Expr Expr
          | NotEqual Expr Expr
          | Not Expr
          | AndStrict Expr Expr
          | AndLazy Expr Expr
          | OrStrict Expr Expr
          | OrLazy Expr Expr
          | Implies Expr Expr
          | Xor Expr Expr
          | ShiftLeft Expr Expr
          | ShiftRight Expr Expr
          | ShiftRightSameSign Expr Expr
          | Set [Expr]
          | Tuple [Expr]
          | List [Expr]
          | PureCallExpr PureCall
          | ImpureCallExpr ImpureCall
    deriving Show

-- | Data-structure to represent a pure function-call
data PureCall = PureCall Ident [Expr]
    deriving Show

-- | Data-structure to represent an impure function-call
data ImpureCall = ImpureCall Ident [Expr]
    deriving Show

-- | Data-structure to represent tab-indentation
newtype Tabs = Tabs Int
    deriving Show

-- | Data-structure to represent a single value
data Value = Integer Integer
           | Real Double
           | Char Char
        --    | String String
           | IdentV String
           | Bool Bool
    deriving Show

-- | Data-structure to represent an identifier
newtype Ident = Ident String
    deriving Show

-- | Stores documentation strings
newtype DocLines = DocLines [DocLine]
    deriving Show
    
-- | Stores single lines of documentation strings
newtype DocLine = DocLine String
    deriving Show

-- instance Functor DocLines where
--     fmap f (DocLines xs) = DocLines (fmap f xs)