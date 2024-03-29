{-# LANGUAGE OverloadedStrings #-}

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
    , getTypeList
    , isValid
    , isValidAnd
    , Purity(..)
    , TypeOp
    , unpackTypes
    ) where

import Data.Aeson (FromJSON, ToJSON, Value(..), (.:), (.=), object, parseJSON, toJSON)
import Data.List (intercalate)
import Data.Map (Map, (!), keys)
import Data.Text (Text, pack, toLower, unpack)

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
    | ERecord (Map String EmperorType) -- ^ Record composite
    | EFunction Purity EmperorType EmperorType -- ^ Function composite
    | Any -- ^ Universal super-type
    | Unit -- ^ Universal sub-type
    deriving (Eq)

instance Show EmperorType where
    show IntP = "int"
    show CharP = "char"
    show BoolP = "bool"
    show RealP = "real"
    show (ESet t) = '{' : show t ++ "}"
    show (EList t) = '[' : show t ++ "]"
    show (ETuple ts) = intercalate "*" $ show <$> ts
    show (ERecord m) = " :: " ++ showMap m
      where
        showMap :: Map String EmperorType -> String
        showMap m' = '{' : intercalate ", " [k ++ " :: " ++ show (m' ! k) | k <- keys m] ++ "}"
    show (EFunction p t1 t2) = show p ++ show t1 ++ " -> " ++ show t2
    show Any = "Any"
    show Unit = "Unit"

-- | Convert a list of type judgements in to a list of emperor types or an error.
unpackTypes :: [TypeJudgementResult] -> Either String [EmperorType]
unpackTypes [] = Right []
unpackTypes (r:rs) =
    case r of
        Valid t ->
            case unpackTypes rs of
                Right ts -> Right (t : ts)
                x -> x
        Invalid m -> Left m

-- | Marker for whether a function is pure or impure
data Purity
    = Pure
    | Impure
    deriving (Eq, Show)

-- | Class of types which describe type operation results
class TypeOp a where
    isValid :: a -> Bool
    -- ^ Indicates whether a type operation has returned a valid result

instance TypeOp TypeCheckResult where
    isValid Pass = True
    isValid (Fail _) = False

instance TypeOp TypeJudgementResult where
    isValid (Valid _) = True
    isValid (Invalid _) = False

-- | Check that a type judgement result is valid and is equal to a given type
isValidAnd :: EmperorType -> TypeJudgementResult -> Bool
isValidAnd _ (Invalid _) = False
isValidAnd t (Valid t')
    | t == t' = True
    | otherwise = False

-- | Get a list of the types mentioned in a function type declaration. Example: 
-- getTypeList a -> b -> c = [a,b,c] 
getTypeList :: EmperorType -> [EmperorType]
getTypeList (EFunction _ t1 t2) = t1 : getTypeList t2
getTypeList x = [x]

instance ToJSON EmperorType where
    toJSON IntP = primitiveToJSON "int"
    toJSON CharP = primitiveToJSON "char"
    toJSON BoolP = primitiveToJSON "bool"
    toJSON RealP = primitiveToJSON "real"
    toJSON (ESet t) = object ["typeConstructor" .= ("ESet" :: Text), "type" .= t]
    toJSON (EList t) = object ["typeConstructor" .= ("EList" :: Text), "type" .= t]
    toJSON (ETuple ts) = object ["typeConstructor" .= ("ETuple" :: Text), "types" .= ts]
    toJSON (ERecord g) = object ["typeConstructor" .= ("ERecord" :: Text), "environment" .= g]
    toJSON (EFunction p t1 t2) =
        object ["typeConstructor" .= ("EFunction" :: Text), "purity" .= p, "inType" .= t1, "outType" .= t2]
    toJSON Any = primitiveToJSON "Any"
    toJSON Unit = primitiveToJSON "Unit"

primitiveToJSON :: String -> Value
primitiveToJSON s = object ["typeConstructor" .= s]

instance FromJSON EmperorType where
    parseJSON (Object o) = do
        c <- o .: "typeConstructor"
        case c :: Text of
            "int" -> return IntP
            "char" -> return CharP
            "bool" -> return BoolP
            "real" -> return RealP
            "Any" -> return Any
            "Unit" -> return Unit
            "ESet" -> ESet <$> o .: "type"
            "EList" -> EList <$> o .: "type"
            "ETuple" -> ETuple <$> o .: "types"
            "ERecord" -> ERecord <$> o .: "environment"
            "EFunction" -> EFunction <$> o .: "purity" <*> o .: "inType" <*> o .: "outType"
            x -> fail $ "Unknown argument type " ++ unpack x
    parseJSON _ = fail "Expecting object value when parsing EmperorType from JSON"

instance ToJSON Purity where
    toJSON p = String $ toLower (pack (show p))

instance FromJSON Purity where
    parseJSON (String s) =
        case s of
            "pure" -> return Pure
            "impure" -> return Impure
            x -> fail $ "Unrecognised purity " ++ unpack x
    parseJSON _ = fail "Expected string value when parsing purity"
