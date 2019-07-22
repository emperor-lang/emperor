{
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-|
Module      : EmperorLexer
Description : Lexer for the emperor language
Copyright   : (c) Edward Jones, 2019
License     : GPL-3
Maintainer  : Edward Jones
Stability   : experimental
Portability : POSIX
Language    : Haskell2010

This module defines the machinery to lexically analyse the Emperor language given in an input string.
-}
module EmperorLexer (Alex, Token(..), lexWrap, alexError, runAlex, AlexPosn) where
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

@lineComment = \/\/ .* \n
@ignoredWhitespace = \\\n

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
"->"                { mkL LGoesTo }
"="                 { mkL LGets }
","                 { mkL LComma}
"("                 { mkL LLParenth }
")"                 { mkL LRParenth }
"["                 { mkL LLBracket }
"]"                 { mkL LRBracket }
"{"                 { mkL LLBrace }
"}"                 { mkL LRBrace }
"@"                 { mkL LImpure }

-- Operators
"+"                { mkL LPlus }
"-"                { mkL LMinus }
"/"                { mkL LDivide }
"%"                { mkL LModulo }
"*"                { mkL LTimes }
"<<"              { mkL LShiftLeft }
">>"                { mkL LShiftRight }
">>>"               { mkL LShiftRightSameSign }
"&"                 { mkL LAndScrict }
"&&"                { mkL LAndLazy }
"|"                { mkL LOrStrict }
"||"              { mkL LOrLazy }
"!"                 { mkL LNot }
"^"                { mkL LXor }
"<"                { mkL LLessThan }
"<="               { mkL LLessThanOrEqual }
">"                 { mkL LGreaterThan }
">="                { mkL LGreaterThanOrEqual }
"=>"                { mkL LImplies }
"=="                { mkL LEqual }
"!="                { mkL LNotEqual }

-- Significant whitespace
@tabs               { mkL LTabs }
\n                  { mkL LEoL }

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
                 | LGoesTo
                 | LGets
                 | LLParenth
                 | LRParenth
                 | LLBracket
                 | LRBracket
                 | LLBrace
                 | LRBrace
                 | LImpure
                 | LPlus
                 | LMinus
                 | LDivide
                 | LModulo
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
                 | LEqual
                 | LNotEqual
                 | LComma
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
                                LGoesTo             -> return (TGoesTo             p)
                                LGets               -> return (TGets               p)
                                LLParenth           -> return (TLParenth           p)
                                LRParenth           -> return (TRParenth           p)
                                LLBracket           -> return (TLBracket           p)
                                LRBracket           -> return (TRBracket           p)
                                LLBrace             -> return (TLBrace             p)
                                LRBrace             -> return (TRBrace             p)
                                LImpure             -> return (TImpure             p)
                                LPlus               -> return (TPlus               p)
                                LMinus              -> return (TMinus              p)
                                LDivide             -> return (TDivide             p)
                                LModulo             -> return (TModulo             p)
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
                                LEqual              -> return (TEqual              p)
                                LNotEqual           -> return (TNotEqual           p)
                                LComma              -> return (TComma              p)
                                LTabs               -> return (TTabs               len p)
                                LEoL                -> return (TEoL                p)

alexEOF :: Alex Token
alexEOF = return TEoF

-- | Wrapper function for the lexer---allows the monadic lexer to be used with 
-- a monadic parser
lexWrap :: (Token -> Alex a) -> Alex a
lexWrap = (alexMonadScan >>=)

-- | Type to represent tokens in the output stream
data Token = TDocAssignmentLine  {                          position :: AlexPosn } -- ^ Documentation field line
           | TDocLine            {                          position :: AlexPosn } -- ^ Generic line of documentation
           | TInteger            { intVal :: Integer,       position :: AlexPosn } -- ^ An integral literal
           | TBool               { isTrue :: Bool,          position :: AlexPosn } -- ^ A boolean literal
           | TReal               { realVal :: Double,       position :: AlexPosn } -- ^ A real/floating-point literal
           | TChar               { charVal :: Char,         position :: AlexPosn } -- ^ A single character literal
           | TIf                 {                          position :: AlexPosn } -- ^ Keyword: @if@
           | TElse               {                          position :: AlexPosn } -- ^ Keyword: @else@
           | TWhile              {                          position :: AlexPosn } -- ^ Keyword: @while@
           | TRepeat             {                          position :: AlexPosn } -- ^ Keyword: @repeat@
           | TWith               {                          position :: AlexPosn } -- ^ Keyword: @with@
           | TSwitch             {                          position :: AlexPosn } -- ^ Keyword: @switch@
           | TFor                {                          position :: AlexPosn } -- ^ Keyword: @for@
           | TIdent              { identifierVal :: String, position :: AlexPosn } -- ^ An identifier
           | TQueue              {                          position :: AlexPosn } -- ^ @<-@
           | TGoesTo             {                          position :: AlexPosn } -- ^ @->@
           | TGets               {                          position :: AlexPosn } -- ^ @=@
           | TLParenth           {                          position :: AlexPosn } -- ^ @(@
           | TRParenth           {                          position :: AlexPosn } -- ^ @)@
           | TLBracket           {                          position :: AlexPosn } -- ^ @[@
           | TRBracket           {                          position :: AlexPosn } -- ^ @]@
           | TLBrace             {                          position :: AlexPosn } -- ^ @{@
           | TRBrace             {                          position :: AlexPosn } -- ^ @}@
           | TImpure             {                          position :: AlexPosn } -- ^ @\@@
           | TPlus               {                          position :: AlexPosn } -- ^ @+@
           | TMinus              {                          position :: AlexPosn } -- ^ @-@
           | TDivide             {                          position :: AlexPosn } -- ^ @/@
           | TModulo             {                          position :: AlexPosn } -- ^ @%@
           | TTimes              {                          position :: AlexPosn } -- ^ @*@
           | TShiftLeft          {                          position :: AlexPosn } -- ^ @<<@
           | TShiftRight         {                          position :: AlexPosn } -- ^ @>>@
           | TShiftRightSameSign {                          position :: AlexPosn } -- ^ @>>>@
           | TAndScrict          {                          position :: AlexPosn } -- ^ @&@
           | TAndLazy            {                          position :: AlexPosn } -- ^ @&&@
           | TOrStrict           {                          position :: AlexPosn } -- ^ @|@
           | TOrLazy             {                          position :: AlexPosn } -- ^ @||@
           | TNot                {                          position :: AlexPosn } -- ^ @!@
           | TXor                {                          position :: AlexPosn } -- ^ @^@
           | TLessThan           {                          position :: AlexPosn } -- ^ @<@
           | TLessThanOrEqual    {                          position :: AlexPosn } -- ^ @<=@
           | TGreaterThan        {                          position :: AlexPosn } -- ^ @>@
           | TGreaterThanOrEqual {                          position :: AlexPosn } -- ^ @>=@
           | TImplies            {                          position :: AlexPosn } -- ^ @=>@
           | TEqual              {                          position :: AlexPosn } -- ^ @==@
           | TNotEqual           {                          position :: AlexPosn } -- ^ @!=@
           | TComma              {                          position :: AlexPosn } -- ^ @,@
           | TTabs               { numTabs :: Int,          position :: AlexPosn } -- ^ @\t@
           | TEoL                {                          position :: AlexPosn } -- ^ @\\n@
           | TEoF                                                                  -- ^ @\\0@
    deriving (Eq, Ord, Show)

-- | AlexPosn is ordered by the total number of characters read (its final field)
instance Ord AlexPosn where
    (AlexPn _ _ c1) < (AlexPn _ _ c2) = c1 < c2
    a <= b = (a < b) || (a == b)

}
