{-|
Module      : Results
Description : Typing results for emperor
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

This module defines valid type structures and the results which may be returned
by the argument checker or resolver.
-}
module Types.Results
    ( EmperorType(..)
    , TypeCheckResult(..)
    , TypeJudgementResult(..)
    , isValid
    , TypeOp
    ) where

import AST (Purity)
import Data.List (concat, intersperse)
import Data.Map ((!), keys, Map)

-- | The result of a typing judgement. This is either an error indicating a
-- problem or a type
data TypeJudgementResult
    = Valid EmperorType -- ^ Valid type
    | Invalid String -- ^ Indicates an incorrect type and
                                          -- gives an explanation
    deriving (Eq, Show)

-- | The result of a type-check; this indicates a valid or invalid relation.
-- Where this is invalid, a message is given.
data TypeCheckResult
    = Pass -- ^ Indicates a correct type statement
    | Fail String -- ^ Indicates an invalid type and provides a reason
    deriving (Eq)

-- | Data to represent all Emperor types
data EmperorType
    = IntP -- ^ Integer primitive
    | CharP -- ^ Character primitive
    | BoolP -- ^ Boolean primitive
    | RealP -- ^ Real number primitive
    | ESet EmperorType -- ^ Set composite
    | EList EmperorType -- ^ List composite
    | ETuple [EmperorType] -- ^ Tuple composite
    | ERecord String (Map String EmperorType) -- ^ Record composite
    | EFunction Purity EmperorType EmperorType -- ^ Function composite
    | Any -- ^ Universal super-type
    | Unit -- ^ Universal sub-type
    deriving Eq

instance Show EmperorType where
    show IntP = "int"
    show CharP = "char"
    show BoolP = "bool"
    show RealP = "real"
    show (ESet t) = '{' : show t ++ "}" 
    show (EList t) = '[' : show t ++ "]"
    show (ETuple ts) = concat (intersperse "*" $ show <$> ts)
    show (ERecord i m) = show i ++ " :: " ++ showMap m
        where
            showMap :: Map String EmperorType -> String
            showMap m' = '{' : (concat $ intersperse ", " [ k ++ " :: " ++ show (m' ! k) | k <- keys m ]) ++ "}"
    show (EFunction p t1 t2) = show p ++ show t1 ++ " -> " ++ show t2
    show Any = "Any"
    show Unit = "Unit"

-- | Class of types which describe type operation results
class TypeOp a where
    isValid :: a -> Bool -- ^ Indicates whether a type operation has returned a valid result

instance TypeOp TypeCheckResult where
    isValid Pass = True
    isValid (Fail _) = False

instance TypeOp TypeJudgementResult where
    isValid (Valid _) = True
    isValid (Invalid _) = False
