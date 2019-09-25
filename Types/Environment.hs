{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : TypeEnvironment
Description : Typing results for emperor
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

This module defines the type environments of Emperor programs.
-}
module Types.Environment
    ( (=>>)
    , filterEnvironment
    , makeEnvironment
    , pushReturnType
    , getEnvironmentPurity
    , getReturnType
    , setPurity
    , has
    , newTypeEnvironment
    , TypeEnvironment(..)
    , insert
    , unsafeGet
    ) where

import Data.Aeson (FromJSON, ToJSON, Value(..), (.:), (.=), object, parseJSON, toJSON)
import Types.Results (EmperorType, Purity(..), TypeJudgementResult(..))
import GHC.Generics (Generic)

-- | An environment which maps names to types
data TypeEnvironment =
    TypeEnvironment [(String, EmperorType)] Purity [EmperorType]
    deriving (Eq, Show)

instance Semigroup TypeEnvironment where
    (TypeEnvironment g p ts) <> (TypeEnvironment g' p' ts') = TypeEnvironment (g ++ g') p'' (ts ++ ts')
        where
            p'' = if p == Pure && p' == Pure then Pure else Impure

instance Monoid TypeEnvironment where
    mempty = newTypeEnvironment
    mappend = (<>)

-- | Creates a fresh type-environment
newTypeEnvironment :: TypeEnvironment
newTypeEnvironment = TypeEnvironment [] Impure []

-- | Check whether a given type environment contains a symbol of the given name
has :: TypeEnvironment -> String -> Bool
has g s =
    case g =>> s of
        Valid _ -> True
        Invalid _ -> False

-- | Get a value from a type environment
(=>>) :: TypeEnvironment -> String -> TypeJudgementResult
TypeEnvironment [] _ _ =>> s = Invalid $ "Identifier " ++ show s ++ " not in current scope"
TypeEnvironment ((i, t):ms) p ts =>> s =
    if s == i
        then Valid t
        else TypeEnvironment ms p ts =>> s

-- | Get a value from a type environment under the assertion that it already
-- exists. This should only be used for types guaranteed to be in the prelude.
unsafeGet :: String -> TypeEnvironment -> EmperorType
unsafeGet s (TypeEnvironment [] _ _) =
    error $
    "Identifier " ++
    show s ++
    " not in current scope, also, the compiler " ++ "programmer used an unsafe operation where it wasn't safe to do so!"
unsafeGet s (TypeEnvironment ((i,t):ms) p ts) =
    if s == i
        then t
        else unsafeGet s (TypeEnvironment ms p ts)

-- | Add a type assertion to the environment
insert :: String -> EmperorType -> TypeEnvironment -> TypeEnvironment
insert s t (TypeEnvironment m p ts) = TypeEnvironment ((s,t) : m) p ts

-- | Create a type environment from a list and other data
makeEnvironment :: [(String, EmperorType)] -> Purity -> [EmperorType] -> TypeEnvironment
makeEnvironment = TypeEnvironment

pushReturnType :: EmperorType -> TypeEnvironment -> TypeEnvironment
pushReturnType t (TypeEnvironment ms p ts) = TypeEnvironment ms p (t : ts)

getReturnType :: TypeEnvironment -> Either String EmperorType
getReturnType (TypeEnvironment _ _ []) = Left "Could not obtain return type outside of function"
getReturnType (TypeEnvironment _ _ ts) = Right $ head ts

setPurity :: Purity -> TypeEnvironment -> TypeEnvironment
setPurity p (TypeEnvironment ms _ ts) = TypeEnvironment ms p ts

getEnvironmentPurity :: TypeEnvironment -> Purity
getEnvironmentPurity (TypeEnvironment _ p _) = p

-- | Filter the entries of the type environment
filterEnvironment :: (String -> Bool) -> TypeEnvironment -> TypeEnvironment
filterEnvironment f (TypeEnvironment ms p ts) = TypeEnvironment (filterEnvironment' f ms) p ts
  where
    filterEnvironment' :: (String -> Bool) -> [(String, EmperorType)] -> [(String, EmperorType)]
    filterEnvironment' _ [] = []
    filterEnvironment' f' ((i, t):ms')
        | f' i = (i, t) : filterEnvironment' f' ms'
        | otherwise = filterEnvironment' f' ms'

instance ToJSON TypeEnvironment where
    toJSON (TypeEnvironment ms p ts) = object [ "env" .= (uncurry KV <$> ms), "purity" .= p, "returns" .= ts]

fromKV :: KV a b -> (a,b)
fromKV kv = (key kv, value kv)

data KV k v = KV { key :: k , value :: v }
    deriving (Generic)

instance (ToJSON k, ToJSON v) => ToJSON (KV k v)

instance (FromJSON k, FromJSON v) => FromJSON (KV k v)

instance FromJSON TypeEnvironment where
    parseJSON (Object v) = TypeEnvironment <$> ((fromKV <$>) <$> (v .: "env")) <*> v .: "purity" <*> v .: "returns"
    parseJSON _ = fail "Expected object when parsing type environment"
