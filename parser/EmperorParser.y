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
    DOCASSIGNMENTLINE   { TDocAssignmentLine  _ }
    DOCLINE             { TDocLine            _ }
    INT                 { TInteger            intVal _ }
    BOOL                { TBool               isTrue _ }
    REAL                { TReal               realVal _ }
    CHAR                { TChar               charVal _ }
    "if"                { TIf                 _ }
    "else"              { TElse               _ }
    "while"             { TWhile              _ }
    "repeat"            { TRepeat             _ }
    "with"              { TWith               _ }
    "switch"            { TSwitch             _ }
    "for"               { TFor                _ }
    IDENT               { TIdent              identifierVal _ }
    "<-"                { TQueue              _ }
    "="                 { TGets               _ }
    "("                 { TLBracket           _ }
    ")"                 { TRBracket           _ }
    "+"                 { TPlus               _ }
    "-"                 { TMinus              _ }
    "/"                 { TDivide             _ }
    "*"                 { TTimes              _ }
    "<<"                { TShiftLeft          _ }
    ">>"                { TShiftRight         _ }
    ">>>"               { TShiftRightSameSign _ }
    "&"                 { TAndScrict          _ }
    "&&"                { TAndLazy            _ }
    "|"                 { TOrStrict           _ }
    "||"                { TOrLazy             _ }
    "!"                 { TNot                _ }
    "^"                 { TXor                _ }
    "<"                 { TLessThan           _ }
    "<="                { TLessThanOrEqual    _ }
    ">"                 { TGreaterThan        _ }
    ">="                { TGreaterThanOrEqual _ }
    "=>"                { TImplies            _ }
    TABS                { TTabs               numTabs _ }
    EOL                 { TEoL                _ }
    -- SECTION     {SectionHeader name _ }
    -- IDENT       {Identifier name _ }
    -- '='         {Assignment _ }
    -- INT         {IntegerNum value _ }
    -- BOOL        {BooleanVal istrue _ }
    -- STRING      {StringTxt text _ }
    -- ';'         {EndAssignment _ }


%%

ast :: {AST}
ast : body                  { AST $1 }

body :: {[BodyLine]}
body : {- empty -}          { [] }
     | bodyLine             { [$1] }
     | bodyLine EOL body    { $1 : $3 }

bodyLine :: {BodyLine}
bodyLine : indentation IDENT "=" value { BodyLine $1 (Ident (identifierVal $2)) $4 }

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
parseError t = alexError $ "Parser error on Token " ++ show t

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