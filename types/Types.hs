module Types (resolveTypes, TypeJudgementResult(..)) where

import AST (AST)
import Resolver ((|>), judge)
import Results (EmperorType, TypeJudgementResult(..))
import TypeEnvironment (TypeEnvironment)

resolveTypes :: AST -> TypeJudgementResult AST
resolveTypes = judge
