module Errors.Errors (Error(..), ErrorResult(..), isError, isResult, fromResult, makeErrorResult, printErrors) where

import           Logger.Logger       (Loggers)
import           Parser.EmperorLexer (AlexPosn)

data Error = Error String (Maybe AlexPosn)

instance Show Error where
    show (Error m p) = case p of
        Just p' -> show p' ++ ':' : m
        Nothing -> m

data ErrorResult a
    = Sin [Error]
    | Dex a

instance Functor ErrorResult where
    fmap f (Dex x) = Dex $ f x
    fmap _ (Sin x) = Sin x

instance Applicative ErrorResult where
    pure = Dex
    Dex f <*> Dex x = Dex $ f x
    Sin x <*> _ = Sin x
    _ <*> Sin x = Sin x

-- instance Applicative a => Monad (ErrorResult a) where
--     return = pure

instance Semigroup a => Semigroup (ErrorResult a) where
    Sin es <> Sin es' = Sin $ es <> es'
    Dex r <> Dex r'   = Dex $ r <> r'
    _ <> Sin es       = Sin es
    Sin es <> _       = Sin es

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

makeErrorResult :: String -> Maybe AlexPosn -> ErrorResult a
makeErrorResult m mp = Sin [Error m mp]

printErrors :: Loggers -> [Error] -> IO ()
printErrors _ []     = return ()
printErrors (err, inf, scc, wrn) (e:es) = do
    err $ show e
    printErrors (err, inf, scc, wrn) es
