{
{-|
Module      : EmperorParser
Description : Parser for the emperor language
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

This module defines the machinery to parse the Emperor language from a token stream generated by the Emperor lexer.
-}
module Parser.EmperorParser (parseEmperor, parseREPL) where

import Parser.AST (AST(..), ModuleHeader(..), Import(..), ImportLocation(..), ImportType(..), Ident(..), ModuleItem(..), FunctionDef(..), FunctionTypeDef(..), TypeComparison(..), BodyBlock(..), SwitchCase(..), BodyLine(..), Assignment(..), Queue(..), Expr(..), Value(..), Call(..))
import Parser.EmperorLexer (Alex, AlexPosn(..), Token(..), lexWrap, alexError, runAlex)
import Types.Results (EmperorType(..), Purity(..))

}

%name parseEmperor ast
%name parseREPL moduleItem

%error { parseError }
%lexer { lexWrap } { TEoF }
%monad { Alex }
%tokentype { Token }

-- Enforce perfection
%expect 0

%token
    -- DOCASSIGNMENTLINE   { TDocAssignmentLine    p }
    -- DOCLINE             { TDocLine              p }
    INT                 { TInteger              intVal p }
    BOOL                { TBool                 isTrue p }
    REAL                { TReal                 realVal p }
    CHAR                { TChar                 charVal p }
    STRING              { TString               stringVal p }
    "if"                { TIf                   p }
    "else"              { TElse                 p }
    "while"             { TWhile                p }
    "repeat"            { TRepeat               p }
    "with"              { TWith                 p }
    "switch"            { TSwitch               p }
    "for"               { TFor                  p }
    "import"            { TImport               p }
    "module"            { TModule               p }
    IDENT               { TIdent                identifierVal p }
    "<-"                { TQueue                p }
    "->"                { TGoesTo               p }
    "="                 { TGets                 p }
    "("                 { TLParenth             p }
    ")"                 { TRParenth             p }
    "["                 { TLBracket             p }
    "]"                 { TRBracket             p }
    "{"                 { TLBrace               p }
    "}"                 { TRBrace               p }
    "+"                 { TPlus                 p }
    "-"                 { TMinus                p }
    "/"                 { TDivide               p }
    "%"                 { TModulo               p }
    "*"                 { TTimes                p }
    "<<"                { TShiftLeft            p }
    ">>"                { TShiftRight           p }
    ">>>"               { TShiftRightSameSign   p }
    "&"                 { TAndScrict            p }
    "&&"                { TAndLazy              p }
    "|"                 { TOrStrict             p }
    "||"                { TOrLazy               p }
    "!"                 { TNot                  p }
    "^"                 { TXor                  p }
    "<"                 { TLessThan             p }
    "<="                { TLessThanOrEqual      p }
    ">"                 { TGreaterThan          p }
    ">="                { TGreaterThanOrEqual   p }
    "=>"                { TImplies              p }
    "=="                { TEqual                p }
    "!="                { TNotEqual             p }
    "@"                 { TImpure               p }
    ","                 { TComma                p }
    ":"                 { TColon                p }
    ";"                 { TPartSeparator        p }
    "int"               { TIntT                 p }
    "bool"              { TBoolT                p }
    "real"              { TRealT                p }
    "char"              { TCharT                p }
    "string"            { TStringT              p }
    "()"                { TUnit                 p }
    "Any"               { TAnyT                 p }
    "<:"                { TIsSubType            p }
    "<~"                { TIsImplementeBy       p }
    "::"                { TIsType               p }
    "class"             { TClass                p }
    "component"         { TComponent            p }
    "#"                 { TBlockSeparator       p }
    "_"                 { TIDC                  p }
    "return"            { TReturn               p }
--     EOL                 { TEoL                  p }

%nonassoc PURE
%nonassoc IMPURE
%nonassoc TYPEDEF
%nonassoc NONTUPLE
%nonassoc TUPLE
%left CALL
%right "->"
%left "||"
%left "&&"
%left "|"
%left "^"
%left "&"
%left "=>"
%left "==" "!="
%left "<" "<=" ">" ">="
%left "<<" ">>" ">>>"
%left "+" "-"
%left "*" "/" "%"
%right NEG "!"
%right "@"
%nonassoc "{" "[" "(" INT REAL CHAR BOOL IDENT STRING

%%

ast :: {AST}
ast : moduleHeader usings moduleBody                 { AST $1 $2 $3 }

moduleHeader :: {ModuleHeader}
moduleHeader : "module" IDENT ";" { Module (Ident (identifierVal $2)) }

-- docs :: {[DocLine]}
-- docs : DOCLINE          { [ $1] }
--      | DOCLINE EOL docs { $1 : $3 }

usings :: {[Import]}
usings : {- empty -}        { [] }
       | using ";" usings   { $1 : $3 }

using :: {Import}
using : "import" usingLabel                      { Import $2 Nothing }
      | "import" usingLabel "(" identList ")"    { Import $2 (Just $4) }

usingLabel :: {ImportLocation}
usingLabel : "<" IDENT ">" { ImportLocation Global (Ident (identifierVal $2)) }
           | STRING        { ImportLocation Local (Ident (stringVal $1)) }

identList :: {[Ident]}
identList : IDENT               { [Ident (identifierVal $1)]}
          | IDENT "," identList { Ident (identifierVal $1) : $3 }

moduleBody :: {[ModuleItem]}
moduleBody : moduleItem             { [$1] }
           | moduleItem moduleBody  { $1 : $2 }

moduleItem :: {ModuleItem}
moduleItem : component    { $1 }
           | typeClass    { $1 }
           | functionDef  { FunctionItem $1 }

component :: {ModuleItem}
component : "component" IDENT maybe(typeComparisons) ":" functionDefs "#" { Component (Ident (identifierVal $2)) $3 $5 }

functionDefs :: {[FunctionDef]}
functionDefs : {- empty -}              { [] }
             | functionDef functionDefs { $1 : $2 }

typeClass :: {ModuleItem}
typeClass : "class" IDENT maybe(typeComparisons) ":" memberTypes "#" { TypeClass (Ident (identifierVal $2)) $3 $5 }

memberTypes :: {[FunctionTypeDef]}
memberTypes : {- empty -}               { [] }
            | functionTypeDef memberTypes    { $1 : $2 }

functionDef :: {FunctionDef}
functionDef : functionTypeDef ";" IDENT functionParamDef ":" body "#" { FunctionDef $1 $4 $6 }

functionTypeDef :: {FunctionTypeDef}
functionTypeDef : IDENT "::" typedef { FunctionTypeDef (Ident (identifierVal $1)) $3 }

functionParamDef :: {[Ident]}
functionParamDef : {- empty -}              { [] }
                 | IDENT functionParamDef   { (Ident (identifierVal $1)) : $2 }

typeComparisons :: {[TypeComparison]}
typeComparisons : typeComparison                        { [$1] }
                | typeComparison "," typeComparisons    { $1 : $3 }

typeComparison :: {TypeComparison}
typeComparison : "<:" IDENT            { IsSubType (Ident (identifierVal $2)) }
               | "<:" IDENT "<~" IDENT { IsSubTypeWithImplementor (Ident (identifierVal $2)) (Ident (identifierVal $4)) }

body :: {[BodyBlock]}
body : bodyBlock        { [$1] }
     | bodyBlock body   { $1 : $2 }

bodyBlock :: {BodyBlock}
bodyBlock : bodyLine ";"                                    { Line $1 }
          | "if" expr ":" body "else" ":" body "#"          { IfElse $2 $4 $7 }
          | "while" expr ":" body "#"                       { While $2 $4 }
          | "for" IDENT "<-" expr ":" body "#"              { For (Ident (identifierVal $2)) $4 $6 }
          | "repeat" expr ":" body "#"                      { Repeat $2 $4 }
          | "with" assignment ":" body "#"                  { With $2 $4 }
          | "switch" expr ":" switchBody "#"                { Switch $2 $4 }

switchBody :: {[SwitchCase]}
switchBody : {- empty -}    { [] }
           | switchCase "#" switchBody { $1 : $3 }

switchCase :: {SwitchCase}
switchCase : expr "->" bodyBlock    { SwitchCase $1 $3 }

bodyLine :: {BodyLine}
bodyLine : assignment            { AssignmentC $1 }
         | queue                 { QueueC $1 }
         | impureCall            { CallC $1 }
         | "return" maybe(expr)  { Return $2 }

assignment :: {Assignment}
assignment : maybe(typedef) IDENT "=" expr { Assignment $1 (Ident (identifierVal $2)) $4 }

queue :: {Queue}
queue : maybe(typedef) IDENT "<-" expr { Queue $1 (Ident (identifierVal $2)) $4 }

typedef :: {EmperorType}
typedef : tupleTypeDef  %prec TYPEDEF   { resolveTuple $1 }

tupleTypeDef :: {[EmperorType]}
tupleTypeDef : nonTupleTypeDef                  %prec NONTUPLE  { [$1] }
             | nonTupleTypeDef "*" tupleTypeDef %prec TUPLE     { $1 : $3}

nonTupleTypeDef :: {EmperorType}
nonTupleTypeDef : "int"                                     { IntP }
                | "bool"                                    { BoolP }
                | "real"                                    { RealP }
                | "char"                                    { CharP }
                | "string"                                  { EList CharP }
                | "()"                                      { Unit }
                | "Any"                                     { Any }
                | "(" typedef ")"                           { $2 }
                | typedef "->" typedef      %prec PURE      { EFunction Pure $1 $3 }
                | "@" typedef "->" typedef  %prec IMPURE    { EFunction Impure $2 $4 }
                | "[" typedef "]"                           { EList $2 }
                | "{" typedef "}"                           { ESet $2 }
                -- | IDENT                     { Ident }

expr :: {Expr}
expr : value                            { Value $1 }
     | "!" expr                         { Not $2 }
     | "-" expr %prec NEG               { Neg $2 }
     | expr "+" expr                    { Add $1 $3 }
     | expr "-" expr                    { Subtract $1 $3 }
     | expr "*" expr                    { Multiply $1 $3 }
     | expr "/" expr                    { Divide $1 $3 }
     | expr "%" expr                    { Modulo $1 $3 }
     | expr "<" expr                    { Less $1 $3 }
     | expr "<=" expr                   { LessOrEqual $1 $3 }
     | expr ">" expr                    { Greater $1 $3 }
     | expr ">=" expr                   { GreaterOrEqual $1 $3 }
     | expr "==" expr                   { Equal $1 $3 }
     | expr "!=" expr                   { NotEqual $1 $3 }
     | expr "&" expr                    { AndStrict $1 $3 }
     | expr "&&" expr                   { AndLazy $1 $3 }
     | expr "|" expr                    { OrStrict $1 $3 }
     | expr "||" expr                   { OrLazy $1 $3 }
     | expr "=>" expr                   { Implies $1 $3 }
     | expr "^" expr                    { Xor $1 $3 }
     | expr "<<" expr                   { ShiftLeft $1 $3 }
     | expr ">>" expr                   { ShiftRight $1 $3 }
     | expr ">>>" expr                  { ShiftRightSameSign $1 $3 }
     | "{" exprList "}"                 { Set $2 }
     | "(" exprList ")"                 { Tuple $2 }
     | "[" exprList "]"                 { List $2 }

value :: {Value}
value : "_"         { IDC }
      | INT         { Integer (intVal $1) }
      | REAL        { Real (realVal $1)}
      | IDENT       { IdentV (Ident (identifierVal $1)) }
      | CHAR        { Char (charVal $1) }
      | BOOL        { Bool (isTrue $1) }
      | STRING      { StringV (stringVal $1) }
      | call %prec CALL { CallV $1 }

call :: {Call}
call : impureCall   { $1 }
     | pureCall     { $1 }

impureCall :: {Call}
impureCall : "@" IDENT "(" exprList ")" { Call Impure (Ident (identifierVal $2)) $4 }

pureCall :: {Call}
pureCall : IDENT "(" exprList ")" { Call Pure (Ident (identifierVal $1)) $3}


exprList :: {[Expr]}
exprList : {- empty -}          { [] }
         | exprListNonZero      { $1 }

exprListNonZero :: {[Expr]}
exprListNonZero : expr                      { [$1] }
                | expr "," exprListNonZero  { $1 : $3 }

maybe(p) : {- empty -} { Nothing }
         | p           { Just $1 }

{

parseError :: Token -> Alex a
parseError t = case t of
    TEoF -> alexError $ "Unexpected EoF"
    t' -> case position t' of
        AlexPn _ l c -> alexError $ show l ++ ":" ++ show c ++ ": " ++ "Parse error on " ++ show t

resolveTuple :: [EmperorType] -> EmperorType
resolveTuple [] = error "The impossible has happened, you seem to have an expression with no type, not even the unit?"
resolveTuple [t] = t
resolveTuple ts = ETuple ts

}
