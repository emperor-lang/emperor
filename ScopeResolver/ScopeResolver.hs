module ScopeResolver.ScopeResolver (Scopable, resolveScope, doScope) where

import           Data.Either           (isLeft, isRight)
import           Logger.Logger         (Loggers)
import           Parser.AST            (AST (..), Assignment (..), BodyBlock (..), BodyLine (..), Call (..), Expr (..),
                                        FunctionDef (..), FunctionTypeDef (..), Ident (..), Import (..),
                                        ImportLocation (..), ModuleHeader (..), ModuleItem (..), Queue (..),
                                        SwitchCase (..), Value (..))
import           System.Exit           (exitFailure)
import           Types.Environment     (TypeEnvironment (..))
import           Types.Imports.Imports (getEnvironment)
import           Types.Results         (EmperorType (..))


data ScopeContext = ScopeContext {
        knownIdents :: [Ident]
    ,   moduleName  :: String
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
                let scopeResult = doScope (ScopeContext { knownIdents = importList, moduleName = s }) (AST (Module s mis p) is bs)
                inf "Done resolving scope"
                case scopeResult of
                    Left m -> do
                        err m
                        exitFailure
                    Right prog -> return prog

class Scopable a where
    doScope :: ScopeContext -> a -> Either String a

class ScopeModifier a where
    doScopeModify :: ScopeContext -> a -> Either String (ScopeContext, a)

instance ScopeModifier a => ScopeModifier [a] where
    doScopeModify c [] = Right (c, [])
    doScopeModify c (a:as) = case doScopeModify c a of
        Right (c', a') -> case doScopeModify c' as of
            Right (c'', as') -> Right (c'', a':as')
            Left m           -> Left m
        Left m -> Left m

instance Scopable AST where
    doScope c (AST (Module s mis p) is ms) = case msr of
            Right ms' -> Right $ AST (Module s mis p) is' ms'
            Left m    -> Left m
        where
            is' :: [Import]
            is' = resolveImportScope <$> is
                where
                    resolveImportScope :: Import -> Import
                    resolveImportScope (Import (ImportLocation t s' p') mis'' p'') = case mis'' of
                        Just imps' -> Import (ImportLocation t s' p') (Just imps') p''
                        Nothing    -> Import (ImportLocation t s' p') (Just $ filter (\(Ident _ m _) -> m == Just s') (knownIdents c)) p''
            msr = unwrapEitherError $ doScope c <$> ms

instance Scopable ModuleItem where
    doScope _ Component{}        = error "Components cannot yet be scoped"
    doScope _ TypeClass{}        = error "Type classes cannot yet be scoped"
    doScope c (FunctionItem d p) = case doScope c d of
        Right d' -> Right $ FunctionItem d' p
        Left msg -> Left msg

instance Scopable FunctionDef where
    doScope c (FunctionDef td is bs p) = case td' of
            Right td'' -> case is' of
                Right is'' -> case bsr of
                    Right (_, bs'') -> Right $ FunctionDef td'' is'' bs'' p
                    Left m          -> Left m
                Left m     -> Left m
            Left m -> Left m
        where
            td' = doScope c td
            is' = let isr = doScope c <$> is in
                if all isRight isr then
                    Right $ fromRight <$> isr
                else
                    Left . fromLeft $ (head . filter isLeft) isr
            bsr = doScopeModify c bs
                -- let bsr = doScope c <$> bs in
                -- if all isRight bsr then
                --     Right $ fromRight <$> bsr
                -- else
                --     Left . fromLeft $ (head . filter isLeft) bsr

instance Scopable FunctionTypeDef where
    doScope c (FunctionTypeDef i t p) = case doScope c i of
        Right i' -> case doScope c t of
            Right t' -> Right $ FunctionTypeDef i' t' p
            Left m   -> Left m
        Left m -> Left m

instance Scopable EmperorType where
    -- There are no user-defined types... yet...
    doScope _ t = Right t

instance Scopable Ident where
    doScope c (Ident i _ p) = case mr of
            Right m -> Right $ Ident i m p
            Left m  -> Left m
        where
            mr = let nameMatches = filter (\(Ident j _ _) -> j == i) (knownIdents c) in
                if null nameMatches then
                    Left $  "Identifier " ++ show i ++ " is not in the current scope"
                else
                    Right . (\(Ident _ m' _) -> m') $ head nameMatches

instance ScopeModifier BodyBlock where
    doScopeModify c (Line l p) = case doScopeModify c l of
        Right (c', l') -> Right (c', Line l' p)
        Left m         -> Left m
    doScopeModify c (IfElse e bs1 bs2 p) = case doScope c e of
        Right e' -> case doScopeModify c bs1 of
            Right (c', bs1') -> case doScopeModify c' bs2 of
                Right (_, bs2') -> Right (c, IfElse e' bs1' bs2' p)
                Left m          -> Left m
            Left m -> Left m
        Left m -> Left m
    doScopeModify c (While e bs p) = case doScope c e of
        Right e' -> case doScopeModify c bs of
            Right (_, bs') -> Right (c, While e' bs' p)
            Left m         -> Left m
        Left m -> Left m
    doScopeModify c (For i e bs p) = case doScope c i of
        Right i' -> case doScope c e of
            Right e' -> let c' = c { knownIdents = i' : knownIdents c } in
                case doScopeModify c' bs of
                    Right (_, bs') -> Right (c, For i' e' bs' p)
                    Left m         -> Left m
            Left m -> Left m
        Left m -> Left m
    doScopeModify c (Repeat e bs p) = case doScope c e of
        Right e' -> case doScopeModify c bs of
            Right (_, bs') -> Right (c, Repeat e' bs' p)
            Left m         -> Left m
        Left m -> Left m
    doScopeModify c (With t i e bs p) = case doScope c t of
        Right t' -> case doScope c i of
            Right i' -> case doScope c e of
                Right e' -> let c' = c { knownIdents = i' : knownIdents c } in
                    case doScopeModify c' bs of
                        Right (_, bs') -> Right (c, With t' i' e' bs' p)
                        Left m         -> Left m
                Left m -> Left m
            Left m -> Left m
        Left m -> Left m
    doScopeModify c (Switch e cs p) = case doScope c e of
            Right e' -> case unwrapEitherError $ doScope c <$> cs of
                Right cs' -> Right (c, Switch e' cs' p)
                Left m    -> Left m
            Left m    -> Left m

instance Scopable SwitchCase where
    doScope c (SwitchCase e b p) = case br of
            Right (_, b') -> SwitchCase <$> (doScope c e) <*> Right b' <*> Right p
            Left m        -> Left m
        where
            br = doScopeModify c b

instance ScopeModifier BodyLine where
    doScopeModify c (AssignmentC a)     = case doScopeModify c a of
        Right (c', a') -> Right (c', AssignmentC a')
        Left m         -> Left m
    doScopeModify c (QueueC q)          = case doScopeModify c q of
        Right (c', q') -> Right (c', QueueC q')
        Left m         -> Left m
    doScopeModify c (CallC c')          = case doScope c c' of
        Right c'' -> Right (c, CallC c'')
        Left m    -> Left m
    doScopeModify c (Return Nothing p)  = Right (c, Return Nothing p)
    doScopeModify c (Return (Just e) p) = case doScope c e of
        Right r -> Right $ (c, Return (Just r) p)
        Left m  -> Left m

instance ScopeModifier Assignment where
    doScopeModify c (Assignment Nothing i e p) = case doScope c i of
        Right i' -> case doScope c e of
            Right e' -> let c' = c { knownIdents = i' : knownIdents c } in
                Right (c', Assignment Nothing i' e' p)
            Left m -> Left m
        Left m -> Left m
    doScopeModify c (Assignment (Just t) i e p) = case doScope c t of
        Right t' -> case doScopeModify c $ Assignment Nothing i e p of
            Right (c', Assignment _ i' e' _) -> Right (c', Assignment (Just t') i' e' p)
            Left m                           -> Left m
        Left m -> Left m

instance ScopeModifier Queue where
    doScopeModify c (Queue Nothing i e p) = case doScope c i of
        Right i' -> case doScope c e of
            Right e' -> let c' = c { knownIdents = i' : knownIdents c } in
                Right (c', Queue Nothing i' e' p)
            Left m -> Left m
        Left m -> Left m
    doScopeModify c (Queue (Just t) i e p) = case doScope c t of
        Right t' -> case doScopeModify c $ Queue Nothing i e p of
            Right (c', Queue _ i' e' _) -> Right (c', Queue (Just t') i' e' p)
            Left m                      -> Left m
        Left m -> Left m



instance Scopable Expr where
    doScope c (Value v p)                  = Value              <$> (doScope c v) <*> Right p
    doScope c (Neg e p)                    = Neg                <$> (doScope c e) <*> Right p
    doScope c (Not e p)                    = Not                <$> (doScope c e) <*> Right p
    doScope c (Add e1 e2 p)                = Add                <$> (doScope c e1) <*> (doScope c e2) <*> Right p
    doScope c (Subtract e1 e2 p)           = Subtract           <$> (doScope c e1) <*> (doScope c e2) <*> Right p
    doScope c (Multiply e1 e2 p)           = Multiply           <$> (doScope c e1) <*> (doScope c e2) <*> Right p
    doScope c (Divide e1 e2 p)             = Divide             <$> (doScope c e1) <*> (doScope c e2) <*> Right p
    doScope c (Modulo e1 e2 p)             = Modulo             <$> (doScope c e1) <*> (doScope c e2) <*> Right p
    doScope c (Less e1 e2 p)               = Less               <$> (doScope c e1) <*> (doScope c e2) <*> Right p
    doScope c (LessOrEqual e1 e2 p)        = LessOrEqual        <$> (doScope c e1) <*> (doScope c e2) <*> Right p
    doScope c (Greater e1 e2 p)            = Greater            <$> (doScope c e1) <*> (doScope c e2) <*> Right p
    doScope c (GreaterOrEqual e1 e2 p)     = GreaterOrEqual     <$> (doScope c e1) <*> (doScope c e2) <*> Right p
    doScope c (Equal e1 e2 p)              = Equal              <$> (doScope c e1) <*> (doScope c e2) <*> Right p
    doScope c (NotEqual e1 e2 p)           = NotEqual           <$> (doScope c e1) <*> (doScope c e2) <*> Right p
    doScope c (AndStrict e1 e2 p)          = AndStrict          <$> (doScope c e1) <*> (doScope c e2) <*> Right p
    doScope c (AndLazy e1 e2 p)            = AndLazy            <$> (doScope c e1) <*> (doScope c e2) <*> Right p
    doScope c (OrStrict e1 e2 p)           = OrStrict           <$> (doScope c e1) <*> (doScope c e2) <*> Right p
    doScope c (OrLazy e1 e2 p)             = OrLazy             <$> (doScope c e1) <*> (doScope c e2) <*> Right p
    doScope c (Implies e1 e2 p)            = Implies            <$> (doScope c e1) <*> (doScope c e2) <*> Right p
    doScope c (Xor e1 e2 p)                = Xor                <$> (doScope c e1) <*> (doScope c e2) <*> Right p
    doScope c (ShiftLeft e1 e2 p)          = ShiftLeft          <$> (doScope c e1) <*> (doScope c e2) <*> Right p
    doScope c (ShiftRight e1 e2 p)         = ShiftRight         <$> (doScope c e1) <*> (doScope c e2) <*> Right p
    doScope c (ShiftRightSameSign e1 e2 p) = ShiftRightSameSign <$> (doScope c e1) <*> (doScope c e2) <*> Right p
    doScope c (Set es p)                   = Set                <$> (unwrapEitherError $ doScope c <$> es) <*> Right p
    doScope c (Tuple es p)                 = Tuple              <$> (unwrapEitherError $ doScope c <$> es) <*> Right p
    doScope c (List es p)                  = List               <$> (unwrapEitherError $ doScope c <$> es) <*> Right p

instance Scopable Value where
    doScope c (IdentV i p) = IdentV <$> (doScope c i) <*> Right p
    doScope c (CallV c' p) = CallV <$> (doScope c c') <*> Right p
    doScope _ x            = Right x

instance Scopable Call where
    doScope c (Call pty i es p) = Call pty <$> (doScope c i) <*> (unwrapEitherError $ doScope c <$> es) <*> Right p

unwrapEitherError :: [Either String a] -> Either String [a]
unwrapEitherError rs =
    if all isRight rs then
        Right $ fromRight <$> rs
    else
        Left . fromLeft . head . filter isLeft $ rs

fromLeft :: Either a b -> a
fromLeft (Left x) = x
fromLeft _        = error "Passed Right _ to fromLeft"

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _         = error "Passed Left _ to fromRight"

