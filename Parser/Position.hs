{-|
Module      : Position
Description : Positional information for the AST
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

This module defines the machinery to parse the Emperor language from a token stream generated by the Emperor lexer.
-}
module Parser.Position
    ( GetPos
    , getPos
    ) where

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
    , ModuleHeader(..)
    , ModuleItem(..)
    , Queue(..)
    , SwitchCase(..)
    , TypeComparison(..)
    , Value(..)
    )
import Parser.EmperorLexer (AlexPosn(..), Token, position)

class GetPos a where
    getPos :: a -> AlexPosn -- ^ Return the position in the input stream

instance GetPos Token where
    getPos = position

instance GetPos AST where
    getPos (AST _ _ _) = AlexPn 1 1 1

instance GetPos ModuleHeader where
    getPos (Module _ p) = p

instance GetPos Import where
    getPos (Import _ _ p) = p

instance GetPos ImportLocation where
    getPos (ImportLocation _ _ p) = p

instance GetPos ModuleItem where
    getPos (Component _ _ _ p) = p
    getPos (TypeClass _ _ _ p) = p
    getPos (FunctionItem _ p) = p

instance GetPos a => GetPos [a] where
    getPos = getPos . head

instance GetPos FunctionDef where
    getPos (FunctionDef _ _ _ p) = p

instance GetPos FunctionTypeDef where
    getPos (FunctionTypeDef _ _ p) = p

instance GetPos TypeComparison where
    getPos (IsSubType _ p) = p
    getPos (IsSubTypeWithImplementor _ _ p) = p

instance GetPos BodyBlock where
    getPos (Line _ p) = p
    getPos (IfElse _ _ _ p) = p
    getPos (While _ _ p) = p
    getPos (For _ _ _ p) = p
    getPos (Repeat _ _ p) = p
    getPos (With _ _ p) = p
    getPos (Switch _ _ p) = p

instance GetPos SwitchCase where
    getPos (SwitchCase _ _ p) = p

instance GetPos BodyLine where
    getPos (AssignmentC a) = getPos a
    getPos (QueueC q) = getPos q
    getPos (CallC c) = getPos c
    getPos (Return _ p) = p

instance GetPos Assignment where
    getPos (Assignment _ _ _ p) = p

instance GetPos Queue where
    getPos (Queue _ _ _ p) = p

instance GetPos Expr where
    getPos (Value _ p) = p
    getPos (Neg _ p) = p
    getPos (Add _ _ p) = p
    getPos (Subtract _ _ p) = p
    getPos (Multiply _ _ p) = p
    getPos (Divide _ _ p) = p
    getPos (Modulo _ _ p) = p
    getPos (Less _ _ p) = p
    getPos (LessOrEqual _ _ p) = p
    getPos (Greater _ _ p) = p
    getPos (GreaterOrEqual _ _ p) = p
    getPos (Equal _ _ p) = p
    getPos (NotEqual _ _ p) = p
    getPos (Not _ p) = p
    getPos (AndStrict _ _ p) = p
    getPos (AndLazy _ _ p) = p
    getPos (OrStrict _ _ p) = p
    getPos (OrLazy _ _ p) = p
    getPos (Implies _ _ p) = p
    getPos (Xor _ _ p) = p
    getPos (ShiftLeft _ _ p) = p
    getPos (ShiftRight _ _ p) = p
    getPos (ShiftRightSameSign _ _ p) = p
    getPos (Set _ p) = p
    getPos (Tuple _ p) = p
    getPos (List _ p) = p

instance GetPos Value where
    getPos (Integer _ p) = p
    getPos (Real _ p) = p
    getPos (Char _ p) = p
    getPos (StringV _ p) = p
    getPos (IdentV _ p) = p
    getPos (Bool _ p) = p
    getPos (CallV _ p) = p

instance GetPos Call where
    getPos (Call _ _ _ p) = p

instance GetPos Ident where
    getPos (Ident _ p) = p
