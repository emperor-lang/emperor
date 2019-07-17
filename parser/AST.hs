module AST where

data AST = AST [BodyLine]
    deriving Show

data BodyLine = BodyLine Tabs Ident Value
    deriving Show
    
data Value = Integer Integer
           | Real Double
           | Char Char
        --    | String String
           | IdentV String
           | Bool Bool
    deriving Show

data Ident = Ident String
    deriving Show

data Tabs = Tabs Int
    deriving Show
-- data Identifier = Ident String
