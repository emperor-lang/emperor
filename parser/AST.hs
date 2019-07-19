module AST where

newtype AST = AST [BodyBlock]
    deriving Show

data BodyBlock = Line BodyLine
               | IfElse Expr [BodyBlock] [BodyBlock]
               | While Expr [BodyBlock]
               | For Ident Expr [BodyBlock]
               | Repeat Expr [BodyBlock]
               | With Assignment [BodyBlock]
               | Switch Expr [SwitchCase]
    deriving Show

-- instance Functor SwitchCases where
--     fmap f (SwitchCases as) = SwitchCases (fmap f as)

data SwitchCase = SwitchCase Expr BodyBlock
    deriving Show
    
-- append :: SwitchCase -> SwitchCases -> SwitchCases
-- append a (SwitchCases as) = SwitchCases (a:as)

data BodyLine = BodyLine Tabs BodyLineContent
    deriving Show

data BodyLineContent = AssignmentC Assignment
                     | QueueC Queue
                     | ImpureCallC ImpureCall
    deriving Show

data Assignment = Assignment Ident Expr
    deriving Show

data Queue = Queue Ident Expr
    deriving Show

data Expr = Value Value
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
          | PureCallExpr PureCall
          | ImpureCallExpr ImpureCall
    deriving Show

data PureCall = PureCall Ident [Expr]
    deriving Show

data ImpureCall = ImpureCall Ident [Expr]
    deriving Show

newtype Tabs = Tabs Int
    deriving Show

data Value = Integer Integer
           | Real Double
           | Char Char
        --    | String String
           | IdentV String
           | Bool Bool
    deriving Show

newtype Ident = Ident String
    deriving Show

-- data Identifier = Ident String
