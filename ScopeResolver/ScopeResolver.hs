module ScopeResolver.ScopeResolver (Scopable, resolveScope, doScope) where

import           Logger.Logger         (Loggers)
import           Parser.AST            (AST (..), Assignment (..), BodyBlock (..), BodyLine (..), Call (..), Expr (..),
                                        FunctionDef (..), FunctionTypeDef (..), Ident (..), Import (..),
                                        ImportLocation (..), ModuleHeader (..), ModuleItem (..), Queue (..),
                                        SwitchCase (..), Value (..))
import           Types.Environment     (TypeEnvironment (..))
import           Types.Imports.Imports (getEnvironment)
import           Types.Results         (EmperorType (..))

data ScopeContext = ScopeContext {
        imports    :: [Ident]
    ,   moduleName :: String
    }

resolveScope :: Loggers -> AST -> IO AST
resolveScope (err, inf, scc, wrn) (AST (Module s mis p) is bs) = do
    inf "Resolving scope..."
    ter <- getEnvironment (err, inf, scc, wrn) is
    case ter of
        Left msg -> do
            err msg
            error "This exception was unhandled :("
        Right (TypeEnvironment ls _ _) -> do
                let importList = fst <$> ls
                let prog = doScope (ScopeContext { imports = importList, moduleName = s }) (AST (Module s mis p) is bs)
                inf "Done resolving scope"
                return prog

class Scopable a where
    doScope :: ScopeContext -> a -> a

instance Scopable AST where
    doScope c (AST (Module s mis p) is ms) = AST (Module s mis p) is' ms'
        where
            is' :: [Import]
            is' = resolveImportScope <$> is
                where
                    resolveImportScope :: Import -> Import
                    resolveImportScope (Import (ImportLocation t s' p') mis'' p'') = case mis'' of
                        Just imps' -> Import (ImportLocation t s' p') (Just imps') p''
                        Nothing    -> Import (ImportLocation t s' p') (Just $ filter (\(Ident _ m _) -> m == Just s') (imports c)) p''
            ms' = doScope c <$> ms

instance Scopable ModuleItem where
    doScope _ Component{}        = error "Components cannot yet be scoped"
    doScope _ TypeClass{}        = error "Type classes cannot yet be scoped"
    doScope c (FunctionItem d p) = FunctionItem (doScope c d) p

instance Scopable FunctionDef where
    doScope c (FunctionDef td is bs p) = FunctionDef td' is' bs' p
        where
            td' = doScope c td
            is' = doScope c <$> is
            bs' = doScope c <$> bs

instance Scopable FunctionTypeDef where
    doScope c (FunctionTypeDef i t p) = FunctionTypeDef i' t' p
        where
            i' = doScope c i
            t' = doScope c t

instance Scopable EmperorType where
    doScope _ t = t

instance Scopable Ident where
    doScope c (Ident i _ p) = Ident i m p
        where
            m = let nameMatches = filter (\(Ident i'' _ _) -> i'' == i) (imports c) in
                if null nameMatches then
                    Just $ moduleName c
                else if length nameMatches >= 2 then
                    error "There are too many matches, and this should have been done as an either :("
                else
                    head $ (\(Ident _ m' _) -> m') <$> nameMatches

instance Scopable BodyBlock where
    doScope c (Line l p)           = Line (doScope c l) p
    doScope c (IfElse e bs1 bs2 p) = IfElse (doScope c e) (doScope c <$> bs1) (doScope c <$> bs2) p
    doScope c (While e bs p)       = While (doScope c e) (doScope c <$> bs) p
    doScope c (For i e bs p)       = For (doScope c i) (doScope c e) (doScope c <$> bs) p
    doScope c (Repeat e bs p)      = Repeat (doScope c e) (doScope c <$> bs) p
    doScope c (With t i e bs p)    = With (doScope c t) (doScope c i) (doScope c e) (doScope c <$> bs) p
    doScope c (Switch e cs p)      = Switch (doScope c e) (doScope c <$> cs) p

instance Scopable SwitchCase where
    doScope c (SwitchCase e b p) = SwitchCase (doScope c e) (doScope c b) p

instance Scopable BodyLine where
    doScope c (AssignmentC a)     = AssignmentC $ doScope c a
    doScope c (QueueC q)          = QueueC $ doScope c q
    doScope c (CallC c')          = CallC $ doScope c c'
    doScope _ (Return Nothing p)  = Return Nothing p
    doScope c (Return (Just e) p) = Return (Just (doScope c e)) p

instance Scopable Assignment where
    doScope c (Assignment Nothing i e p)  = Assignment Nothing (doScope c i) (doScope c e) p
    doScope c (Assignment (Just t) i e p) = Assignment (Just (doScope c t)) (doScope c i) (doScope c e) p

instance Scopable Queue where
    doScope c (Queue Nothing i e p)  = Queue Nothing (doScope c i) (doScope c e) p
    doScope c (Queue (Just t) i e p) = Queue (Just (doScope c t)) (doScope c i) (doScope c e) p

instance Scopable Expr where
    doScope c (Value v p)                  = Value (doScope c v) p
    doScope c (Neg e p)                    = Neg (doScope c e) p
    doScope c (Add e1 e2 p)                = Add (doScope c e1) (doScope c e2) p
    doScope c (Subtract e1 e2 p)           = Subtract (doScope c e1) (doScope c e2) p
    doScope c (Multiply e1 e2 p)           = Multiply (doScope c e1) (doScope c e2) p
    doScope c (Divide e1 e2 p)             = Divide (doScope c e1) (doScope c e2) p
    doScope c (Modulo e1 e2 p)             = Modulo (doScope c e1) (doScope c e2) p
    doScope c (Less e1 e2 p)               = Less (doScope c e1) (doScope c e2) p
    doScope c (LessOrEqual e1 e2 p)        = LessOrEqual (doScope c e1) (doScope c e2) p
    doScope c (Greater e1 e2 p)            = Greater (doScope c e1) (doScope c e2) p
    doScope c (GreaterOrEqual e1 e2 p)     = GreaterOrEqual (doScope c e1) (doScope c e2) p
    doScope c (Equal e1 e2 p)              = Equal (doScope c e1) (doScope c e2) p
    doScope c (NotEqual e1 e2 p)           = NotEqual (doScope c e1) (doScope c e2) p
    doScope c (Not e p)                    = Not (doScope c e) p
    doScope c (AndStrict e1 e2 p)          = AndStrict (doScope c e1) (doScope c e2) p
    doScope c (AndLazy e1 e2 p)            = AndLazy (doScope c e1) (doScope c e2) p
    doScope c (OrStrict e1 e2 p)           = OrStrict (doScope c e1) (doScope c e2) p
    doScope c (OrLazy e1 e2 p)             = OrLazy (doScope c e1) (doScope c e2) p
    doScope c (Implies e1 e2 p)            = Implies (doScope c e1) (doScope c e2) p
    doScope c (Xor e1 e2 p)                = Xor (doScope c e1) (doScope c e2) p
    doScope c (ShiftLeft e1 e2 p)          = ShiftLeft (doScope c e1) (doScope c e2) p
    doScope c (ShiftRight e1 e2 p)         = ShiftRight (doScope c e1) (doScope c e2) p
    doScope c (ShiftRightSameSign e1 e2 p) = ShiftRightSameSign (doScope c e1) (doScope c e2) p
    doScope c (Set es p)                   = Set (doScope c <$> es) p
    doScope c (Tuple es p)                 = Tuple (doScope c <$> es) p
    doScope c (List es p)                  = List (doScope c <$> es) p

instance Scopable Value where
    doScope c (IdentV i p) = IdentV (doScope c i) p
    doScope c (CallV c' p) = CallV (doScope c c') p
    doScope _ x            = x

instance Scopable Call where
    doScope c (Call pty i es p) = Call pty (doScope c i) (doScope c <$> es) p

