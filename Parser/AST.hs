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
module Parser.AST
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

import qualified Data.Aeson as A (FromJSON, ToJSON, Value(Object, String), (.:), (.=), object, parseJSON, toJSON)
import Data.Text (pack, unpack)
import Parser.EmperorLexer (AlexPosn(..))
import Types.Results (EmperorType(..), Purity(..))

-- | Data type to represent the abstract syntax tree for a single module. This is specified by its name, its imports and its code.
data AST =
    AST ModuleHeader [Import] [ModuleItem]
    deriving (Show)

-- | A single module header
data ModuleHeader =
    Module Ident AlexPosn
    deriving (Show)

-- | A single imported file
data Import =
    Import ImportLocation (Maybe [Ident]) AlexPosn
    deriving (Show)

-- | Location of an import and how to treat it
data ImportLocation =
    ImportLocation ImportType Ident AlexPosn
    deriving (Show)

instance A.ToJSON ImportLocation where
    toJSON (ImportLocation t (Ident i _) p) = A.object ["importType" A..= t, "import" A..= pack i, "location" A..= p]

instance A.FromJSON ImportLocation where
    parseJSON (A.Object v) =
        ImportLocation <$> v A..: "importType" <*> (Ident <$> v A..: "import" <*> v A..: "location") <*>
        v A..: "location"
    parseJSON _ = fail "Expected object when parsing import datum"

-- | The type of an import
data ImportType
    = Local -- ^ Indicates a file in the current project
    | Global -- ^ Indicates a file in the global installation
    deriving (Show)

instance A.ToJSON ImportType where
    toJSON Local = A.String "local"
    toJSON Global = A.String "global"

instance A.FromJSON ImportType where
    parseJSON (A.String s) =
        case s of
            "local" -> return Local
            "global" -> return Global
            _ -> fail $ "Got " ++ unpack s ++ " when parsing import type (expected \"local\"/\"global\""
    parseJSON _ = fail "Expected string when parsing import type"

-- | Describes a single named item in the module
data ModuleItem
    = Component Ident (Maybe [TypeComparison]) [FunctionDef] AlexPosn
    | TypeClass Ident (Maybe [TypeComparison]) [FunctionTypeDef] AlexPosn
    | FunctionItem FunctionDef AlexPosn
    deriving (Show)

-- | Describes the definition of a function
data FunctionDef =
    FunctionDef FunctionTypeDef [Ident] [BodyBlock] AlexPosn
    deriving (Show)

-- | Describes the definition of the type of a function
data FunctionTypeDef =
    FunctionTypeDef Ident EmperorType AlexPosn
    deriving (Show)

-- | Describes an explicit type assertion
data TypeComparison
    = IsSubType Ident AlexPosn
    | IsSubTypeWithImplementor Ident Ident AlexPosn
    deriving (Show)

-- | Represents a single construction in the body of a function. This may be a
-- further construct or just a single line
data BodyBlock
    = Line BodyLine AlexPosn -- ^ A single line of code
    | IfElse Expr [BodyBlock] [BodyBlock] AlexPosn -- ^ An if-else block
    | While Expr [BodyBlock] AlexPosn -- ^ A while-loop
    | For Ident Expr [BodyBlock] AlexPosn -- ^ A for-loop
    | Repeat Expr [BodyBlock] AlexPosn -- ^ A repeat-loop
    | With Assignment [BodyBlock] AlexPosn -- ^ A resource acquisition
    | Switch Expr [SwitchCase] AlexPosn -- ^ A switch-case statement
    deriving (Show)

-- | Data-struecture for the switch-case statement
data SwitchCase =
    SwitchCase Expr BodyBlock AlexPosn
    deriving (Show)

-- | Data-structure for a single body-line
data BodyLine
    = AssignmentC Assignment
    | QueueC Queue
    | CallC Call
    | Return (Maybe Expr) AlexPosn
    deriving (Show)

-- | Data-structure to represent an assignment statement
data Assignment =
    Assignment (Maybe EmperorType) Ident Expr AlexPosn
    deriving (Show)

-- | Data-structure to represent an queue statement
data Queue =
    Queue (Maybe EmperorType) Ident Expr AlexPosn
    deriving (Show)

-- | Data-structure to represent an expression
data Expr
    = Value Value AlexPosn
    | Neg Expr AlexPosn
    | Add Expr Expr AlexPosn
    | Subtract Expr Expr AlexPosn
    | Multiply Expr Expr AlexPosn
    | Divide Expr Expr AlexPosn
    | Modulo Expr Expr AlexPosn
    | Less Expr Expr AlexPosn
    | LessOrEqual Expr Expr AlexPosn
    | Greater Expr Expr AlexPosn
    | GreaterOrEqual Expr Expr AlexPosn
    | Equal Expr Expr AlexPosn
    | NotEqual Expr Expr AlexPosn
    | Not Expr AlexPosn
    | AndStrict Expr Expr AlexPosn
    | AndLazy Expr Expr AlexPosn
    | OrStrict Expr Expr AlexPosn
    | OrLazy Expr Expr AlexPosn
    | Implies Expr Expr AlexPosn
    | Xor Expr Expr AlexPosn
    | ShiftLeft Expr Expr AlexPosn
    | ShiftRight Expr Expr AlexPosn
    | ShiftRightSameSign Expr Expr AlexPosn
    | Set [Expr] AlexPosn
    | Tuple [Expr] AlexPosn
    | List [Expr] AlexPosn
    deriving (Show)

-- | Data-structure to represent a single value
data Value
    = IDC AlexPosn
    | Integer Integer AlexPosn
    | Real Double AlexPosn
    | Char Char AlexPosn
    | StringV String AlexPosn
    | IdentV Ident AlexPosn
    | Bool Bool AlexPosn
    | CallV Call AlexPosn
    deriving (Show)

-- | Represents the use of a function
data Call =
    Call Purity Ident [Expr] AlexPosn
    deriving (Show)

-- | Data-structure to represent an identifier
data Ident =
    Ident String AlexPosn
    deriving (Eq, Ord, Show)
