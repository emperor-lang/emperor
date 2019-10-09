module Errors.Errors (Error(..), ErrorResult(..), isError, isResult, fromResult) where

import           Parser.EmperorLexer (AlexPosn)

data Error = Error String AlexPosn

instance Show Error where
    show (Error m p) = show p ++ ':' : m

data ErrorResult a
    = Sin [Error]
    | Dex a

instance Semigroup a => Semigroup (ErrorResult a) where
    Sin es <> Sin es' = Sin $ es <> es'
    Dex _ <> Sin es   = Sin es
    Sin es <> Dex _   = Sin es
    Dex r <> Dex r'   = Dex $ r <> r'

instance Monoid a => Monoid (ErrorResult a) where
    mempty = Dex mempty

isError :: ErrorResult a -> Bool
isError (Sin _) = True
isError (Dex _) = False

isResult :: ErrorResult a -> Bool
isResult (Dex _) = True
isResult (Sin _) = False

fromResult :: ErrorResult a -> a
fromResult (Dex x) = x
fromResult _       = error "Attempted to extract result from error"
