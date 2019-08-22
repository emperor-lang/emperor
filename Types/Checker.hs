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
    , FunctionDef(..)
    , FunctionTypeDef(..)
    , Ident(..)
    , ModuleItem(..)
    , Queue(..)
    , SwitchCase(..)
    )
import Types.Environment (TypeEnvironment(..), (=>>), fromList, insert, unsafeGet)
import Types.Imports.Imports (getLocalEnvironment)
import Types.Judger ((|>))
import Types.Results
    ( EmperorType(BoolP, EFunction, IntP, Unit)
    , Purity(..)
    , TypeCheckResult(..)
    , TypeJudgementResult(..)
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
    g >- (FunctionDef (FunctionTypeDef _ t _) is bs _) = g' `check` bs
      where
        g' :: TypeEnvironment
        g' = insert "return" returnType (fromList $ ((\(Ident i _) -> i) <$> is) `zip` paramTypes) <> g
          where
            paramTypes = init $ getTypeList t
            returnType = last $ getTypeList t

-- | A switch-case is type-checked by considering the type of its expression and applying this to its contents
instance TypeCheck SwitchCase where
    g >- (SwitchCase e b _) =
        case g |> e of
            Valid (EFunction p ti to) ->
                case g |- p <: Pure of
                    Pass ->
                        case g =>> "caseExpr" of
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

check :: TypeEnvironment -> [BodyBlock] -> TypeCheckResult
check _ [] = Pass
check g (b':bs') =
    case b' of
        Line l _ ->
            case l of
                AssignmentC (Assignment mt (Ident i _) e _) ->
                    case mt of
                        Just t ->
                            case g =>> i of
                                Valid _ ->
                                    Fail $
                                    "Variable " ++
                                    i ++ " already exists in the current scope. Name shadowing is forbidden."
                                Invalid _ ->
                                    case g |> e of
                                        Valid t' ->
                                            case g |- t' <: t of
                                                Pass ->
                                                    let g' = insert i t g
                                                     in g' `check` bs'
                                                x -> x
                                        Invalid m -> Fail m
                        Nothing ->
                            case g =>> i of
                                Valid t ->
                                    case g |> e of
                                        Valid t' ->
                                            case g |- t <: t' of
                                                Pass -> g `check` bs'
                                                x -> x
                                        Invalid m -> Fail m
                                Invalid m -> Fail m
                QueueC (Queue mt (Ident i _) e _) ->
                    case mt of
                        Just t ->
                            case g |> e of
                                Valid t' ->
                                    case g |- t' <: t of
                                        Pass ->
                                            let g' = insert i t g
                                             in g' `check` bs'
                                        x -> x
                                Invalid m -> Fail m
                        Nothing ->
                            case g =>> i of
                                Valid t ->
                                    case g |> e of
                                        Valid t' ->
                                            case g |- t <: t' of
                                                Pass -> g `check` bs'
                                                x -> x
                                        Invalid m -> Fail m
                                Invalid m -> Fail m
                CallC (Call p (Ident i q) es q') ->
                    case g |- Impure <: p of
                        Pass ->
                            case g |> Call p (Ident i q) es q' of
                                Valid _ -> Pass
                                Invalid m -> Fail m
                        Fail _ -> Fail "Pure bare calls do not have any effect."
                Return Nothing _ ->
                    case g =>> "return" of
                        Valid Unit -> Pass
                        Valid t -> Fail $ "Cannot return the unit, expected " ++ show t
                        Invalid m -> Fail m
                Return (Just e) _ ->
                    case g =>> "return" of
                        Valid t ->
                            case g |> e of
                                Valid t' -> g |- t' <: t
                                Invalid m -> Fail m
                        Invalid _ -> Fail "Return statement found outside of function"
        IfElse e as bs _ ->
            case g |> e of
                Valid t ->
                    case g |- t <: BoolP of
                        Pass ->
                            case g `check` as of
                                Pass -> g `check` bs
                                x -> x
                        x -> x
                Invalid m -> Fail m
        While e as _ ->
            case g |> e of
                Valid t ->
                    case g |- t <: BoolP of
                        Pass -> g `check` as
                        x -> x
                Invalid m -> Fail m
        For (Ident i _) e as _ ->
            case g |> e of
                Valid t ->
                    let g' = insert i t g
                     in g' `check` as
                Invalid m -> Fail m
        Repeat e as _ ->
            case g |> e of
                Valid t ->
                    case g |- t <: IntP of
                        Pass -> g `check` as
                        x -> x
                Invalid m -> Fail m
        With (Assignment Nothing (Ident i _) e _) bs _ ->
            case g |> e of
                Valid t ->
                    let g' = insert i t g
                     in g' `check` bs
                Invalid m -> Fail m
        With (Assignment (Just t) (Ident i _) e _) bs _ ->
            case g |> e of
                Valid t' ->
                    case g |- t' <: t of
                        Pass ->
                            let g' = insert i t' g
                             in g' `check` bs
                        x -> x
                Invalid m -> Fail m
        Switch e cs _ ->
            case g |> e of
                Valid t ->
                    case g |- t <: BoolP of
                        Pass ->
                            let g' = insert "caseExpr" t g
                             in let trs = (g' >-) <$> cs
                                 in if all isValid trs
                                        then Pass
                                        else head $ filter (not . isValid) trs
                        x -> x
                Invalid m -> Fail m
