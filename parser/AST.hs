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
module AST
    ( Assignment(..)
    , AST(..)
    , BodyBlock(..)
    , BodyLine(..)
    , BodyLineContent(..)
    , Expr(..)
    , getPurity
    , Ident(..)
    , Import(..)
    , ImportLocation(..)
    , ImportType(..)
    , ModuleHeader(..)
    , PartialCall(..)
    , Purity(..)
    , Queue(..)
    , SwitchCase(..)
    , Tabs(..)
    , Value(..)
    ) where

-- | Data type to represent the abstract syntax tree for a single module. This is specified by its name, its imports and its code.
data AST =
    AST ModuleHeader [Import] [BodyBlock]
    deriving (Show)

-- | A single module header
data ModuleHeader = Module Ident
    deriving Show
-- | A single imported file
data Import =
    Import ImportLocation (Maybe [Ident])
    deriving Show

-- | Location of an import and how to treat it
data ImportLocation =
    ImportLocation ImportType Ident
    deriving Show

-- | The type of an import
data ImportType
    = Local -- ^ Indicates a file in the current project
    | Global -- ^ Indicates a file in the global installation
    deriving Show

-- | Represents a single construction in the body of a function. This may be a
-- further construct or just a single line
data BodyBlock
    = Line BodyLine -- ^ A single line of code
    | IfElse Expr [BodyBlock] [BodyBlock] -- ^ An if-else block
    | While Expr [BodyBlock] -- ^ A while-loop
    | For Ident Expr [BodyBlock] -- ^ A for-loop
    | Repeat Expr [BodyBlock] -- ^ A repeat-loop
    | With Assignment [BodyBlock] -- ^ A resource acquisition
    | Switch Expr [SwitchCase] -- ^ A switch-case statement
    deriving (Show)

-- instance Functor SwitchCases where
--     fmap f (SwitchCases as) = SwitchCases (fmap f as)
-- | Data-struecture for the switch-case statement
data SwitchCase =
    SwitchCase Expr BodyBlock
    deriving (Show)

-- append :: SwitchCase -> SwitchCases -> SwitchCases
-- append a (SwitchCases as) = SwitchCases (a:as)
-- | Data-structure for a single body-line
data BodyLine =
    BodyLine Tabs BodyLineContent
    deriving (Show)

-- | Data-structure for the contents of a single line
data BodyLineContent
    = AssignmentC Assignment
    | QueueC Queue
    | CallC PartialCall
    deriving (Show)

-- | Data-structure to represent an assignment statement
data Assignment =
    Assignment Ident Expr
    deriving (Show)

-- | Data-structure to represent an queue statement
data Queue =
    Queue Ident Expr
    deriving (Show)

-- | Data-structure to represent an expression
data Expr
    = Value Value
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
    deriving (Show)

-- | Represents the use of a function
data PartialCall
    = PartialApplication PartialCall Expr
    | CallIdentifier Purity Ident
    deriving (Show)

-- | Get the purity of a partial application call
getPurity :: PartialCall -> Purity
getPurity (PartialApplication c _) = getPurity c
getPurity (CallIdentifier p _) = p

-- | Marker for whether a function is pure or impure
data Purity
    = Pure
    | Impure
    deriving (Eq, Show)

-- | Data-structure to represent tab-indentation
newtype Tabs =
    Tabs Int
    deriving (Show)

-- | Data-structure to represent a single value
data Value
    = Integer Integer
    | Real Double
    | Char Char
        --    | String String
    -- | IdentV String
    | Bool Bool
    | Call PartialCall
    deriving (Show)

-- | Data-structure to represent an identifier
newtype Ident =
    Ident String
    deriving (Show)
