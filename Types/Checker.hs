{-|
Module      : Types.Checker
Description : Type checking for Emperor
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

This module defines type-checking for Emperor.
-}
module Types.Checker
    ( TypeCheck
    , (>-)
    ) where

import Data.Monoid ((<>))
import Parser.AST
    ( AST(..)
    , Assignment(..)
    , BodyBlock(..)
    , BodyLine(..)
    , Call(..)
    , Expr
    , FunctionDef(..)
    , FunctionTypeDef(..)
    , Ident(..)
    , ModuleItem(..)
    , Queue(..)
    , SwitchCase(..)
    )
import Types.Environment (TypeEnvironment(..), (=>>), getReturnType, makeEnvironment, getEnvironmentPurity, insert, unsafeGet)
import Types.Imports.Imports (getLocalEnvironment)
import Types.Judger ((|>))
import Types.Results
    ( EmperorType(..)
    , Purity(..)
    , TypeCheckResult(..)
    , TypeJudgementResult(..)
    , getPurity
    , getTypeList
    , isValid
    )
import Types.SubTyping ((<:), (|-))

-- | Describes objects which may be type-checked.
class TypeCheck a where
    infix 2 >-
    -- | Check the assertion that a given @a@ has valid type
    (>-) :: TypeEnvironment -> a -> TypeCheckResult

-- | An AST can be type-checked by applying its environment to its contents.
instance TypeCheck AST where
    g >- (AST m is bs) =
        let trs = (g' >-) <$> bs
         in if all isValid trs
                then Pass
                else head $ filter (not . isValid) trs
            -- The type environment to use
      where
        g' = getLocalEnvironment (AST m is bs) <> g

-- | Module item may be type-checked by considering its contents
instance TypeCheck ModuleItem where
    _ >- Component {} = Fail "Components have not yet been implemented" -- TODO: Implement components
    _ >- TypeClass {} = Fail "Type classes have not yet been implemented" -- TODO: Implement type classes
    g >- (FunctionItem f _) = g >- f

-- | A function definition may be type-checked using its parameters applied to its contents
instance TypeCheck FunctionDef where
    g >- (FunctionDef (FunctionTypeDef _ t _) is bs _) = if length paramTypesMap /= length is then
                Fail "Number of parameters does not correspond to the input type"
            else case getPurity t of
                Left m  -> Fail m
                Right p -> check (makeEnvironment paramTypesMap p [returnType] <> g) bs
        where
            paramTypesMap = filter (not . isUnit) $ ((\(Ident i _) -> i) <$> is) `zip` (init $ getTypeList t)
            isUnit (_,Unit) = True
            isUnit _ = False
            returnType = last $ getTypeList t

checkLine :: TypeEnvironment -> BodyLine -> Either String TypeEnvironment
checkLine g l = case l of
        AssignmentC (Assignment (Just t) (Ident i _) e _) -> checkEnvironmentMutatorLine t i e
        AssignmentC (Assignment Nothing (Ident i _) e _) -> checkEnvironmentNonMutatorLine i e
        QueueC (Queue (Just t) (Ident i _) e _) -> checkEnvironmentMutatorLine t i e
        QueueC (Queue Nothing (Ident i _) e _) -> checkEnvironmentNonMutatorLine i e
        CallC (Call pty i es p) -> case g |- Impure <: pty of
            Pass -> case g |> (Call pty i es p) of
                Valid _ -> Right g
                Invalid m -> Left m
            Fail m -> Left m
        Return Nothing _ -> case getReturnType g of
            Left _ -> Right g
            Right _ -> Left "Expected return value in this environment"
        Return (Just e) _ -> case getReturnType g of
            Right t -> case g |> e of
                Valid t' -> case g |- t' <: t of
                    Pass -> Right g
                    _ -> Left "This environment does not return a value"
                Invalid m -> Left m
            Left m -> Left m
    where
        checkEnvironmentMutatorLine :: EmperorType -> String -> Expr -> Either String TypeEnvironment
        checkEnvironmentMutatorLine t i e = if t == Unit then
                Left "Assignments to the unit are valid but pointless, please remove this."
            else
                case g =>> i of
                Invalid _ -> case g |> e of
                    Valid t' -> case g |- t' <: t of
                        Pass -> Right $ insert i t g
                        Fail m -> Left m
                    Invalid m -> Left m
                Valid _ -> Left $ "Identifier " ++ show i ++ " already exists in the current scope"

        checkEnvironmentNonMutatorLine :: String -> Expr -> Either String TypeEnvironment
        checkEnvironmentNonMutatorLine i e = case g =>> i of
            Valid t -> case g |> e of
                Valid t' -> case g |- t' <: t of
                    Pass -> Right g
                    Fail m -> Left m
                Invalid m -> Left m
            Invalid m -> Left m

check :: TypeEnvironment -> [BodyBlock] -> TypeCheckResult
check _ [] = Pass
check g (b:bs) =
    case b of
        Line l _ -> case checkLine g l of
            Left m -> Fail m
            Right g' -> check g' bs
        IfElse c bs1 bs2 _ -> case g |> c of
            Valid t -> case g |- t <: BoolP of
                Pass -> case check g bs1 of
                    Pass -> case check g bs2 of
                        Pass -> g `check` bs
                        x -> x
                    x -> x
                x -> x
            Invalid m -> Fail m
        While c bs' _ -> case g |> c of
            Valid t -> case g |- t <: BoolP of
                Pass -> case check g bs' of
                    Pass -> check g bs
                    x -> x
                x -> x
            Invalid m -> Fail m
        For (Ident i _) e bs' _ -> case g |> e of
            Valid t -> case g |- t <: EList Any of
                Pass -> case check (insert i t g) bs' of
                    Pass -> check g bs
                    x -> x
                x -> x
            Invalid m -> Fail m
        Repeat e bs' _ -> case g |> e of
            Valid t -> case g |- t <: IntP of
                Pass -> case check g bs' of
                    Pass -> check g bs
                    x -> x
                x -> x
            Invalid m -> Fail m
        With t (Ident i _) e bs' _ -> if getEnvironmentPurity g == Pure then
                Fail "With statements are not allowed in a pure scope"
            else case g |> e of
                Valid t' -> case g |- t' <: t of
                    Pass -> case check (insert i t g) bs' of
                        Pass -> check g bs
                        x -> x
                    x -> x
                Invalid m -> Fail m
        Switch e bs' _ -> case g |> e of
            Valid t -> let tcrs = (insert ".switch_case" t g >-) <$> bs' in
                if all isValid tcrs then
                    check g bs
                else
                    head $ filter (not . isValid) tcrs
            Invalid m -> Fail m

-- | A switch-case is type-checked by considering the type of its expression and applying this to its contents
instance TypeCheck SwitchCase where
    g >- (SwitchCase e b _) =
        case g |> e of
            Valid (EFunction p ti to) ->
                case g |- p <: Pure of
                    Pass ->
                        case g =>> ".switch_case" of
                            Valid t ->
                                case g |- t <: ti of
                                    Pass ->
                                        case g |- to <: BoolP of
                                            Pass -> g `check` [b]
                                            Fail _ -> Fail "Return type of case guard is not a boolean! "
                                    x -> x
                            Invalid _ -> Fail "Missing expression type for case?"
                    Fail _ -> Fail "Case guards must be pure, see `man emperor` to report if this is bad"
            Valid t ->
                Fail $
                "Case guard had type " ++
                show t ++ " but type " ++ show (unsafeGet "caseExpr" g) ++ " -> " ++ show BoolP
            Invalid m -> Fail m
