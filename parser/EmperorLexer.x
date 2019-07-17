{
module EmperorLexer where

import AST
}

%wrapper "monad"

$alpha = [A-Za-z]
$num = [0-9]
$alphaNum = [$alpha$num]

@tabs = \t+
@spaces = \ +

@ident = $alpha $alphaNum*
@int = $num+
@real = $num+ \. $num+
@bool = (true) | (false)
@char = \'$alphaNum\'
@string = "$alphaNum*"

@docLineStart = @tabs? "///"
@docAssignmentLine = @docLineStart @spaces? "~" @ident ("(" @ident ")")? ":" .*
@docLine = @docLineStart .*

-- @string = \"\"

@lineComment = \/\/ .*
@ignoredWhitespace = \\\n


-- $spaces = [\ \t]
-- $alpha = [a-zA-Z]
-- $digits = [0-9]
-- $alnum = [$alpha$digits]

-- @identifier = $alpha $alnum*

-- @comment = \#.*

-- @integer = $digits+

-- @boolean = (true) | (false)

-- @string = \"[^\"]*\"

:-

-- Things to ignore
@spaces             ;
@lineComment        ;
@ignoredWhitespace  ;

-- Documentation
@docAssignmentLine  { mkL LDocAssignmentLine }
@docLine            { mkL LDocLine }

-- Values
@int                { mkL LInteger }
@bool               { mkL LBool }
@real               { mkL LReal }
@char               { mkL LChar }
-- @string         { mkL LString }

-- Keywords
"if"                { mkL LIf }
"else"              { mkL LElse }
"while"             { mkL LWhile }
"repeat"            { mkL LRepeat }
"with"              { mkL LWith }
"switch"            { mkL LSwitch }
"for"               { mkL LFor }

-- Identifiers
@ident              { mkL LIdent }

-- Syntax things
"<-"                { mkL LQueue }
"="                 { mkL LGets }
"("                 { mkL LLBracket }
")"                 { mkL LRBracket }

-- Operators
"\+"                { mkL LPlus }
"\-"                { mkL LMinus }
"\/"                { mkL LDivide }
"\*"                { mkL LTimes }
"\<\<"              { mkL LShiftLeft }
">>"                { mkL LShiftRight }
">>>"               { mkL LShiftRightSameSign }
"&"                 { mkL LAndScrict }
"&&"                { mkL LAndLazy }
"\|"                { mkL LOrStrict }
"\|\|"              { mkL LOrLazy }
"!"                 { mkL LNot }
"\^"                { mkL LXor }
"\<"                { mkL LLessThan }
"\<="               { mkL LLessThanOrEqual }
">"                 { mkL LGreaterThan }
">="                { mkL LGreaterThanOrEqual }
"=>"                { mkL LImplies }

-- Significant whitespace
@tabs               { mkL LTabs }
\n                  { mkL LEoL }


-- @integer    { mkL LInteger }
-- @boolean    { mkL LBoolean }
-- @string     { mkL LString }

-- @identifier  { mkL LIdentifier }

-- \[@identifier\] { mkL LSection }

-- =           { mkL LAssign }

-- \;          { mkL LEndAssign }
-- @comment    ;
-- [\ \t \n]+  ;


{

data LexemeClass = LDocAssignmentLine
                 | LDocLine
                 | LInteger
                 | LBool
                 | LReal
                 | LChar
                 | LIf
                 | LElse
                 | LWhile
                 | LRepeat
                 | LWith
                 | LSwitch
                 | LFor
                 | LIdent
                 | LQueue
                 | LGets
                 | LLBracket
                 | LRBracket
                 | LPlus
                 | LMinus
                 | LDivide
                 | LTimes
                 | LShiftLeft
                 | LShiftRight
                 | LShiftRightSameSign
                 | LAndScrict
                 | LAndLazy
                 | LOrStrict
                 | LOrLazy
                 | LNot
                 | LXor
                 | LLessThan
                 | LLessThanOrEqual
                 | LGreaterThan
                 | LGreaterThanOrEqual
                 | LImplies
                 | LTabs
                 | LEoL
    deriving (Eq, Show)


mkL :: LexemeClass -> AlexInput -> Int -> Alex Token
mkL c (p, _, _, str) len = let t = take len str in
                            case c of 
                                LDocAssignmentLine  -> return (TDocAssignmentLine  p)
                                LDocLine            -> return (TDocLine            p)
                                LInteger            -> return (TInteger            ((read t) :: Integer) p)
                                LBool               -> return (TBool               (if t == "true" then True else False) p)
                                LReal               -> return (TReal               ((read t) :: Double) p)
                                LChar               -> return (TChar               (t !! 1) p)
                                LIf                 -> return (TIf                 p)
                                LElse               -> return (TElse               p)
                                LWhile              -> return (TWhile              p)
                                LRepeat             -> return (TRepeat             p)
                                LWith               -> return (TWith               p)
                                LSwitch             -> return (TSwitch             p)
                                LFor                -> return (TFor                p)
                                LIdent              -> return (TIdent              t p)
                                LQueue              -> return (TQueue              p)
                                LGets               -> return (TGets               p)
                                LLBracket           -> return (TLBracket           p)
                                LRBracket           -> return (TRBracket           p)
                                LPlus               -> return (TPlus               p)
                                LMinus              -> return (TMinus              p)
                                LDivide             -> return (TDivide             p)
                                LTimes              -> return (TTimes              p)
                                LShiftLeft          -> return (TShiftLeft          p)
                                LShiftRight         -> return (TShiftRight         p)
                                LShiftRightSameSign -> return (TShiftRightSameSign p)
                                LAndScrict          -> return (TAndScrict          p)
                                LAndLazy            -> return (TAndLazy            p)
                                LOrStrict           -> return (TOrStrict           p)
                                LOrLazy             -> return (TOrLazy             p)
                                LNot                -> return (TNot                p)
                                LXor                -> return (TXor                p)
                                LLessThan           -> return (TLessThan           p)
                                LLessThanOrEqual    -> return (TLessThanOrEqual    p)
                                LGreaterThan        -> return (TGreaterThan        p)
                                LGreaterThanOrEqual -> return (TGreaterThanOrEqual p)
                                LImplies            -> return (TImplies            p)
                                LTabs               -> return (TTabs               len p)
                                LEoL                -> return (TEoL                p)
                        --    in case c of
                        --         LInteger -> return (IntegerNum ((read t) :: Integer) p)
                        --         LBoolean -> return (BooleanVal (if t == "true"
                        --                                            then True
                        --                                            else False
                        --                                        ) p)
                        --         LString -> return (StringTxt (take (length t - 2) (drop 1 t)) p)
                        --         LIdentifier -> return (Identifier t p)
                        --         LSection -> return (SectionHeader (take (length t - 2) (drop 1 t)) p)
                        --         LAssign -> return (Assignment p)
                        --         LEndAssign -> return (EndAssignment p)


alexEOF :: Alex Token
alexEOF = return TEoF

lexWrap :: (Token -> Alex a) -> Alex a
lexWrap = (alexMonadScan >>=)

data Token = TDocAssignmentLine  {                          position :: AlexPosn }
           | TDocLine            {                          position :: AlexPosn }
           | TInteger            { intVal :: Integer,       position :: AlexPosn }
           | TBool               { isTrue :: Bool,          position :: AlexPosn }
           | TReal               { realVal :: Double,       position :: AlexPosn }
           | TChar               { charVal :: Char,         position :: AlexPosn }
           | TIf                 {                          position :: AlexPosn }
           | TElse               {                          position :: AlexPosn }
           | TWhile              {                          position :: AlexPosn }
           | TRepeat             {                          position :: AlexPosn }
           | TWith               {                          position :: AlexPosn }
           | TSwitch             {                          position :: AlexPosn }
           | TFor                {                          position :: AlexPosn }
           | TIdent              { identifierVal :: String, position :: AlexPosn }
           | TQueue              {                          position :: AlexPosn }
           | TGets               {                          position :: AlexPosn }
           | TLBracket           {                          position :: AlexPosn }
           | TRBracket           {                          position :: AlexPosn }
           | TPlus               {                          position :: AlexPosn }
           | TMinus              {                          position :: AlexPosn }
           | TDivide             {                          position :: AlexPosn }
           | TTimes              {                          position :: AlexPosn }
           | TShiftLeft          {                          position :: AlexPosn }
           | TShiftRight         {                          position :: AlexPosn }
           | TShiftRightSameSign {                          position :: AlexPosn }
           | TAndScrict          {                          position :: AlexPosn }
           | TAndLazy            {                          position :: AlexPosn }
           | TOrStrict           {                          position :: AlexPosn }
           | TOrLazy             {                          position :: AlexPosn }
           | TNot                {                          position :: AlexPosn }
           | TXor                {                          position :: AlexPosn }
           | TLessThan           {                          position :: AlexPosn }
           | TLessThanOrEqual    {                          position :: AlexPosn }
           | TGreaterThan        {                          position :: AlexPosn }
           | TGreaterThanOrEqual {                          position :: AlexPosn }
           | TImplies            {                          position :: AlexPosn }
           | TTabs               { numTabs :: Int,          position :: AlexPosn }
           | TEoL                {                          position :: AlexPosn }
           | TEoF
    deriving (Eq, Ord, Show)
-- data Token = SectionHeader {identifier :: String, position :: AlexPosn} |
--              Identifier {name :: String, position :: AlexPosn}          |
--              Assignment {position :: AlexPosn}                          |
--              EndAssignment {position :: AlexPosn}                       |
--              IntegerNum {value :: Integer, position :: AlexPosn}        |
--              BooleanVal {istrue :: Bool, position :: AlexPosn}          |
--              StringTxt  {text :: String, position :: AlexPosn}          |
--              Eof
--     deriving (Eq, Show, Ord)

instance Ord AlexPosn where
    (AlexPn a _ _) < (AlexPn b _ _) = a < b
    a <= b = (a < b) || (a == b)

}

-- {
-- module EmperorLexer where
-- }

-- %wrapper "monad"

-- -- Whitespace
-- $space = [\ \t]

-- $alpha = [a-zA-Z]
-- $num = [0-9]
-- $alphaNum = [$alpha$num]
-- -- $operator = [<>+-*/]

-- @var = $alpha$alphaNum+
-- @comment = \/\/.*
-- @int = $num+

-- -- $spaces = [\ \t]
-- -- $alpha = [a-zA-Z]
-- -- $digits = [0-9]
-- -- $alnum = [$alpha$digits]

-- -- @identifier = $alpha $alnum*

-- -- @comment = \#.*

-- -- @integer = $digits+

-- -- @boolean = (true) | (false)

-- -- @string = \"[^\"]*\"


-- tokens :-
--         @comment        ;
--         $space+         ;
--         "<-"            { mkL LAssign }
--         '+'             { mkL LPlus }
--         -- '-'             { mkL LMinus }
--         -- '*'             { mkL LTimes }
--         -- '/'             { mkL LDivide }
--         @int            { mkL LInteger }
--         @var            { mkL LVariable }
--         [\n]            ;
--         -- .               { alexError "Unrecognised character" } -- Causes error!

-- {
--     -- data LexemeClass = LVariable | LInteger | LPlus | LMinus | LTimes | LDivide | LAssign | LEOF
--     data LexemeClass = LVariable | LInteger | LPlus | LAssign | LEOF
--         deriving (Eq, Show)
--     -- data LexemeClass = LInteger | LBoolean | LString | LIdentifier | LSection | LAssign | LEndAssign | LEOF
--     -- data LexemeClass = LInteger | LBoolean | LIdentifier | LSection | LAssign | LEndAssign | LEOF
--     --     deriving (Eq, Show)

--     lexWrap :: (Token -> Alex a) -> Alex a
--     lexWrap = (alexMonadScan >>=)

--     mkL :: LexemeClass -> AlexInput -> Int -> Alex Token
--     mkL c (p, _, _, str) len = let t = take len str
--         in case c of
--             LVariable -> return $ TokenVariable (read t) p
--             LInteger -> return $ TokenInteger ((read t) :: Int) p
--             LPlus -> return $ TokenPlus p
--             -- LMinus -> return $ TokenMinus p
--             -- LTimes -> return $ TokenTimes p
--             -- LDivide -> return $ TokenDivide p
--             LAssign -> return $ TokenAssign p
--             LEOF -> return TokenEoF
--             -- LInteger -> return (IntegerNum ((read t) :: Integer) p)
--             -- LBoolean -> return (BooleanVal (if t == "true"
--             --                                 then True
--             --                                 else False
--             --                             ) p)
--             -- -- LString -> return (StringTxt (take (length t - 2) (drop 1 t)) p)
--             -- LIdentifier -> return (Identifier t p)
--             -- LSection -> return (SectionHeader (take (length t - 2) (drop 1 t)) p)
--             -- LAssign -> return (Assignment p)
--             -- LEndAssign -> return (EndAssignment p)
--             -- LEOF -> return Eof


--     -- No idea why I have to write this myself. Documentation doesn't mention it.
--     alexEOF :: Alex Token
--     alexEOF = return TokenEoF

--     data Token = TokenVariable { name :: String, position :: AlexPosn }
--                | TokenInteger { value :: Int, position :: AlexPosn }
--                | TokenPlus { position :: AlexPosn }
--                --    | TokenMinus { position :: AlexPosn }
--                --    | TokenTimes { position :: AlexPosn }
--                --    | TokenDivide { position :: AlexPosn }
--                | TokenAssign { position :: AlexPosn }
--                | TokenEoF
--         deriving (Eq, Ord, Show)

--     instance Ord AlexPosn where
--         (AlexPn a _ _) < (AlexPn b _ _) = a < b
--         a <= b = not (a > b)

--     -- data Token = SectionHeader {identifier :: String, position :: AlexPosn} |
--     --             Identifier {name :: String, position :: AlexPosn}           |
--     --             Assignment {position :: AlexPosn}                           |
--     --             EndAssignment {position :: AlexPosn}                        |
--     --             IntegerNum {value :: Integer, position :: AlexPosn}         |
--     --             BooleanVal {istrue :: Bool, position :: AlexPosn}           |
--     --             Eof
--     --     deriving (Eq, Show)
-- }