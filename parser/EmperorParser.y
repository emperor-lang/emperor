{
module EmperorParser where

import AST
import EmperorLexer

-- TODO:    Figure out why a Monad would be good? 
--          Create a monad which allows the context to be assigned


-- data EmperorParserResult = EmperorParserResult Alexposn

-- instance Monad EmperorParserResult where
--     (>>=) = asdf
--     return = []
}


%name parseEmperor ast
-- %name parseREPL ...
-- TODO: Add a repl parser

%error { parseError }
%lexer  { lexWrap } { TEoF }
%monad { Alex }
%tokentype { Token }
%token
    DOCASSIGNMENTLINE   { TDocAssignmentLine    _ }
    DOCLINE             { TDocLine              _ }
    INT                 { TInteger              intVal _ }
    BOOL                { TBool                 isTrue _ }
    REAL                { TReal                 realVal _ }
    CHAR                { TChar                 charVal _ }
    "if"                { TIf                   _ }
    "else"              { TElse                 _ }
    "while"             { TWhile                _ }
    "repeat"            { TRepeat               _ }
    "with"              { TWith                 _ }
    "switch"            { TSwitch               _ }
    "for"               { TFor                  _ }
    IDENT               { TIdent                identifierVal _ }
    "<-"                { TQueue                _ }
    "->"                { TGoesTo               _ }
    "="                 { TGets                 _ }
    "("                 { TLParenth             _ }
    ")"                 { TRParenth             _ }
    "["                 { TLBracket             _ }
    "]"                 { TRBracket             _ }
    "{"                 { TLBrace               _ }
    "}"                 { TRBrace               _ }
    "+"                 { TPlus                 _ }
    "-"                 { TMinus                _ }
    "/"                 { TDivide               _ }
    "*"                 { TTimes                _ }
    "<<"                { TShiftLeft            _ }
    ">>"                { TShiftRight           _ }
    ">>>"               { TShiftRightSameSign   _ }
    "&"                 { TAndScrict            _ }
    "&&"                { TAndLazy              _ }
    "|"                 { TOrStrict             _ }
    "||"                { TOrLazy               _ }
    "!"                 { TNot                  _ }
    "^"                 { TXor                  _ }
    "<"                 { TLessThan             _ }
    "<="                { TLessThanOrEqual      _ }
    ">"                 { TGreaterThan          _ }
    ">="                { TGreaterThanOrEqual   _ }
    "=>"                { TImplies              _ }
    "=="                { TEqual                _ }
    "!="                { TNotEqual             _ }
    "@"                 { TImpure               _ }
    ","                 { TComma                _ }
    TABS                { TTabs                 numTabs _ }
    EOL                 { TEoL                  _ }

%left "<" "<=" ">" ">="
%left "==" "!="
%left "&" "|" "^"
%left "&&" "||"
%left "=>"
%left "<<" ">>" ">>>"
%left "+" "-"
%left "*" "/"
%left NEG
%right "@"
%right "!"

%%

ast :: {AST}
ast : body                  { AST $1 }

body :: {[BodyBlock]}
body : {- empty -}          { [] }
     | bodyBlock EOL body   { $1 : $3 }

bodyBlock :: {BodyBlock}
bodyBlock : bodyLine                        { Line $1 }
          | "if" expr EOL body "else" body  { IfElse $2 $4 $6 }
          | "while" expr EOL body           { While $2 $4 }
          | "for" IDENT "<-" expr EOL body  { For (Ident (identifierVal $2)) $4 $6 }
          | "repeat" expr EOL body          { Repeat $2 $4 }
          | "with" assignment EOL body      { With $2 $4 }
          | "switch" expr EOL switchBody    { Switch $2 $4 }

switchBody :: {[SwitchCase]}
switchBody : {- empty -}    { [] }
           | switchCase EOL switchBody { $1 : $3 }

switchCase :: {SwitchCase}
switchCase : expr "->" bodyBlock    { SwitchCase $1 $3 }

bodyLine :: {BodyLine}
bodyLine : indentation bodyLineContent {BodyLine $1 $2}

bodyLineContent :: {BodyLineContent}
bodyLineContent : assignment            { AssignmentC $1 }
                | queue                 { QueueC $1 }
                | impureCall            { ImpureCallC $1 }

assignment :: {Assignment}
assignment : IDENT "=" expr { Assignment (Ident (identifierVal $1)) $3 } 

queue :: {Queue}
queue : IDENT "<-" expr { Queue (Ident (identifierVal $1)) $3 }

expr :: {Expr}
expr : value                            { Value $1 }
     | "!" expr                         { Not $2 }
     | "-" expr %prec NEG               { Neg $2 }
     | expr "+" expr                    { Add $1 $3 }
     | expr "-" expr                    { Subtract $1 $3 }
     | expr "*" expr                    { Multiply $1 $3 }
     | expr "/" expr                    { Divide $1 $3 }
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
     | "(" exprList ")"                 { Tuple $2 }
     | "[" exprList "]"                 { List $2 }
     | pureCall                         { PureCallExpr $1 }
     | impureCall                       { ImpureCallExpr $1 }

pureCall :: {PureCall}
pureCall : IDENT "(" exprList ")"       { PureCall (Ident (identifierVal $1)) $3 }

impureCall :: {ImpureCall}
impureCall : "@" IDENT "(" exprList ")"  { ImpureCall (Ident (identifierVal $2)) $4 }

exprList :: {[Expr]}
exprList : {- empty -}          { [] }
         | exprListNonZero      { $1 }

exprListNonZero :: {[Expr]}
exprListNonZero : expr                      { [$1] }
                | expr "," exprListNonZero  { $1 : $3 }

indentation :: {Tabs}
indentation : {- empty -} { Tabs 0 }
            | TABS        { Tabs (numTabs $1) }

value :: {Value}
value : INT     { Integer (intVal $1) }
      | REAL    { Real (realVal $1)}
      | IDENT   { IdentV (identifierVal $1) }
      | CHAR    { Char (charVal $1) }
      | BOOL    { Bool (isTrue $1) }

-- ConfigFile :: {[Section]}
--            : SequenceOfSections                    {reverse $1}
--            | error                                 {[]}

-- SequenceOfSections :: {[Section]}
--                    : {- empty -}                   {   []  }
--                    | SequenceOfSections Section    {$2 : $1}


-- Section :: {Section}
--         : SECTION SectionBody                      {Section (identifier $1) (reverse $2)}


-- SectionBody :: {[(String,Value)]}
--             : {- empty -}        {[]}
--             | SectionBody AssignmentLine ';' {$2 : $1}


-- AssignmentLine :: {(String, Value)}
--                : IDENT '=' Value      {(name $1, $3)}


-- Value :: {Value}
--       : INT         {IntV (value $1)}
--       | BOOL        {BoolV (istrue $1)}
--       | STRING      {StringV (text $1)}

{

-- data Value = IntV Integer | BoolV Bool | StringV String
--     deriving (Eq, Show)

-- data Section = Section String [(String, Value)]
--     deriving (Eq, Show)

-- data IniFile = IniFile [Section]
--     deriving (Eq, Show)

parseError :: Token -> Alex a
parseError t = alexError $ "Parser error on token " ++ show t

}

-- {
-- module EmperorParser where

-- import EmperorLexer
-- }

-- %name parseEmperor
-- %lexer { lexWrap } { TokenEoF }
-- %tokentype { Token }
-- %error { parseError }
-- %monad { Alex }

-- %token
--     var         { TokenVariable name _ }
--     int         { TokenInteger value _ }
--     "<-"        { TokenAssign _ }
--     '+'         { TokenPlus _ }
--     -- '-'          { TokenMinus _ }
--     -- '*'         { TokenTimes _ }
--     -- '/'         { TokenDivide _ }

-- -- -- %left '+' '-'
-- -- -- %left '*' '/'
-- -- -- %nonassoc ASSIGN

-- -- %%

-- -- expr : expr '+' expr

-- -- {
--     -- parseError :: [Token] -> Alex a
-- -- parseError _ = fail "Parse error, can this be a Monad?"
-- -- }

-- -- %name parseEmperor
-- -- %lexer { lexwrap } { EoF }
-- -- %tokentype { Token }
-- -- %error { parseError }

-- -- %token
-- --     let { TokenLet }
-- --     in  { TokenIn }
-- --     int { TokenInt $$ }
-- --     var { TokenSym $$ }
-- --     '=' { TokenEq }
-- --     '+' { TokenPlus }
-- --     '-' { TokenMinus }
-- --     '*' { TokenTimes }
-- --     '/' { TokenDiv }
-- --     '(' { TokenLParen }
-- --     ')' { TokenRParen }

-- %right in
-- -- %nonassoc '>' '<'
-- %left '+' '-'
-- %left '*' '/'
-- %left NEG

-- %%
-- -- TODO: remove precidence by making explicit in the grammar

-- Exp :: {Exp}
--     : var                       { Variable (name $1) }
--     | int                       { Integer (value $1) }
--     | Exp '+' Exp               { Plus $1 $3 }
--     -- | Exp '-' Exp               { Minus $1 $3 }
--     -- | Exp '*' Exp               { Times $1 $3 }
--     -- | Exp '/' Exp               { Divide $1 $3 }

-- -- Exp : let var '=' Exp in Exp { Let $2 $4 $6 }
-- --     | Exp '+' Exp            { Plus $1 $3 }
-- --     | Exp '-' Exp            { Minus $1 $3 }
-- --     | Exp '*' Exp            { Times $1 $3 }
-- --     | Exp '/' Exp            { Div $1 $3 }
-- --     | '(' Exp ')'            { $2 }
-- --     | '-' Exp %prec NEG      { Negate $2 }
-- --     | int                    { Int $1 }
-- --     | var                    { Var $1 }

-- {

-- parseError :: [Token] -> Alex a
-- parseError _ = alexError "Parse error"

-- data Exp = Variable String
--          | Integer Int
--          | Plus Exp Exp
--         --  | Minus Exp Exp
--         --  | Times Exp Exp
--         --  | Divide Exp Exp
-- -- data Exp = Let String Exp Exp
-- --          | Plus Exp Exp
-- --          | Minus Exp Exp
-- --          | Times Exp Exp
-- --          | Div Exp Exp
-- --          | Negate Exp
-- --          | Brack Exp
-- --          | Int Int
-- --          | Var String
--          deriving Show

-- -- parse :: String -> Either String a 
-- -- parse s = runAlex s parseEmperor

-- -- main :: IO () 
-- -- main = do {
-- --     case parse of 
-- --         Left msg -> putStrLn msg
-- --         Right _ -> putStrLn "It worked?!"
-- -- }
-- }