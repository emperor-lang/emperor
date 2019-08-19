{-# LANGUAGE OverloadedStrings #-}
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
    ) where

import qualified Data.Aeson as A (FromJSON, Value(Object, String), ToJSON, (.:), (.=), parseJSON, object, toJSON)
import Data.Text (pack, unpack)
import Types.Results (EmperorType(..), Purity(..))

-- | Data type to represent the abstract syntax tree for a single module. This is specified by its name, its imports and its code.
data AST =
    AST ModuleHeader [Import] [ModuleItem]
    deriving (Show)

-- | A single module header
data ModuleHeader =
    Module Ident
    deriving (Show)

-- | A single imported file
data Import =
    Import ImportLocation (Maybe [Ident])
    deriving (Show)

-- | Location of an import and how to treat it
data ImportLocation =
    ImportLocation ImportType Ident
    deriving (Show)

instance A.ToJSON ImportLocation where
    toJSON (ImportLocation t (Ident i)) = A.object [ "importType" A..= t, "import" A..= pack i ]

instance A.FromJSON ImportLocation where
    parseJSON (A.Object v) = ImportLocation <$> v A..: "importType"
                                          <*> (Ident <$> v A..: "import")
    parseJSON _ = fail "Expected object when parsing import datum"

-- | The type of an import
data ImportType
    = Local -- ^ Indicates a file in the current project
    | Global -- ^ Indicates a file in the global installation
    deriving (Show)

instance A.ToJSON ImportType where
    toJSON Local = A.String $ "local"
    toJSON Global = A.String $ "global"

instance A.FromJSON ImportType where
    parseJSON (A.String s) = case s of
            "local" -> return Local
            "global" -> return Global
            _ -> fail $ "Got " ++ unpack s ++ " when parsing import type (expected \"local\"/\"global\""
    parseJSON _ = fail "Expected string when parsing import type"

-- | Describes a single named item in the module
data ModuleItem
    = Component Ident (Maybe [TypeComparison]) [FunctionDef]
    | TypeClass Ident (Maybe [TypeComparison]) [FunctionTypeDef]
    | FunctionItem FunctionDef
    deriving (Show)

-- | Describes the definition of a function
data FunctionDef
    = FunctionDef FunctionTypeDef [Ident] [BodyBlock]
    deriving (Show)

-- | Describes the definition of the type of a function
data FunctionTypeDef = FunctionTypeDef Ident EmperorType
    deriving (Show)

-- | Describes an explicit type assertion
data TypeComparison
    = IsSubType Ident
    | IsSubTypeWithImplementor Ident Ident
    deriving (Show)

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

-- | Data-structure for a single body-line
data BodyLine
    = AssignmentC Assignment
    | QueueC Queue
    | CallC Call
    | Return Expr
    deriving (Show)

-- | Data-structure to represent an assignment statement
data Assignment =
    Assignment (Maybe EmperorType) Ident Expr
    deriving (Show)

-- | Data-structure to represent an queue statement
data Queue =
    Queue (Maybe EmperorType) Ident Expr
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

-- | Data-structure to represent a single value
data Value
    = IDC
    | Integer Integer
    | Real Double
    | Char Char
    -- | String String
    | IdentV Ident
    | Bool Bool
    | CallV Call
    deriving (Show)

-- | Represents the use of a function
data Call
    = Call Purity Ident [Expr]
    deriving (Show)

-- | Data-structure to represent an identifier
newtype Ident =
    Ident String
    deriving (Eq, Ord, Show)
