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
    , fromList
    , has
    , newTypeEnvironment
    , TypeEnvironment(..)
    , insert
    , unsafeGet
    ) where

import Data.Aeson (FromJSON, ToJSON, Value(..), parseJSON, toJSON)
import Types.Results (EmperorType, TypeJudgementResult(..))

-- | An environment which maps names to types
newtype TypeEnvironment =
    TypeEnvironment [(String, EmperorType)]
    deriving (Eq, Show)

instance Monoid TypeEnvironment where
    mempty = newTypeEnvironment
    mappend (TypeEnvironment g) (TypeEnvironment g') = TypeEnvironment $ g ++ g'

-- | Creates a fresh type-environment
newTypeEnvironment :: TypeEnvironment
newTypeEnvironment = TypeEnvironment []

-- | Check whether a given type environment contains a symbol of the given name
has :: TypeEnvironment -> String -> Bool
has g s = case g =>> s of
    Valid _ -> True
    Invalid _ -> False

-- | Get a value from a type environment
(=>>) :: TypeEnvironment -> String -> TypeJudgementResult
TypeEnvironment [] =>> s = Invalid $ "Identifier " ++ show s ++ " not in current scope"
TypeEnvironment ((i,t):ms) =>> s = if s == i
    then Valid t
    else TypeEnvironment ms =>> s

-- | Get a value from a type environment under the assertion that it already
-- exists. This should only be used for types guaranteed to be in the prelude.
unsafeGet :: String -> TypeEnvironment -> EmperorType
unsafeGet s (TypeEnvironment []) = error $ "Identifier " ++ show s ++ " not in current scope, also, the programmer used an unsafe operation!"
unsafeGet s (TypeEnvironment ((i,t):ms)) = if s == i
    then t
    else unsafeGet s (TypeEnvironment ms)

-- | Add a type assertion to the environment
insert :: String -> EmperorType -> TypeEnvironment -> TypeEnvironment
insert s t (TypeEnvironment m) = TypeEnvironment ((s,t):m)

-- | Create a type environment from a list
fromList :: [(String,EmperorType)] -> TypeEnvironment
fromList = TypeEnvironment

-- | Filter the entries of the type environment
filterEnvironment :: (String -> Bool) -> TypeEnvironment -> TypeEnvironment
filterEnvironment f (TypeEnvironment ms) = TypeEnvironment $ filterEnvironment' f ms
    where
        filterEnvironment' :: (String -> Bool) -> [(String,EmperorType)] -> [(String,EmperorType)]
        filterEnvironment' _ [] = []
        filterEnvironment' f' ((i,t):ms')
            | f' i = (i,t) : filterEnvironment' f' ms'
            | otherwise = filterEnvironment' f' ms'

instance ToJSON TypeEnvironment where
    toJSON (TypeEnvironment ms) = toJSON ms

instance FromJSON TypeEnvironment where
    parseJSON (Array o) = do
        ms <- parseJSON (Array o)
        return $ TypeEnvironment ms
    parseJSON _ = fail "Expected array object when parsing type environment"
