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

@newline = \r\n | \r | \n
@tabs = \t+
@spaces = \ +

@ident = $alpha $alphaNum*
@int = $num+
@real = $num+ \. $num+
@bool = (true) | (false)
@char = \'$alphaNum\'
-- @string = "$alphaNum*"
@partSeparator = ";" @newline?

@docStart = @tabs? "/*" \n?
@docEnd = @tabs? "*/" \n?
@docLineStart = @tabs? " *" (@spaces | @tabs)?

@lineComment = \/\/ .* @newline @tabs?
@ignoredWhitespace = \\ @newline

:-

-- Things to ignore
<0>                 @spaces             ;
<0>                 @ignoredWhitespace  ;
<0>                 @lineComment        ;

-- Documentation
<0>                 @docStart           { begin docs }
<docs>              @docLineStart       { begin docLineContents }
<docLineContents>   .*                  { mkL LDocLine }
<docLineContents>   \n                  { begin docs }
<docLineContents>   @docEnd             { begin 0 }
<docs>              @docEnd             { begin 0 }
-- <0>                 @docLine            { mkL LDocLine }

-- Values
<0>                 @int                { mkL LInteger }
<0>                 @bool               { mkL LBool }
<0>                 @real               { mkL LReal }
<0>                 @char               { mkL LChar }
-- <0>                 @string             { mkL LString }

-- Keywords
<0>                 "if"                { mkL LIf }
<0>                 "else"              { mkL LElse }
<0>                 "while"             { mkL LWhile }
<0>                 "repeat"            { mkL LRepeat }
<0>                 "with"              { mkL LWith }
<0>                 "switch"            { mkL LSwitch }
<0>                 "for"               { mkL LFor }

-- Identifiers
<0>                 @ident              { mkL LIdent }

-- Syntax things
<0>                 @partSeparator      { mkL LPartSeparator }
<0>                 "<-"                { mkL LQueue }
<0>                 "->"                { mkL LGoesTo }
<0>                 "="                 { mkL LGets }
<0>                 ","                 { mkL LComma}
<0>                 "("                 { mkL LLParenth }
<0>                 ")"                 { mkL LRParenth }
<0>                 "["                 { mkL LLBracket }
<0>                 "]"                 { mkL LRBracket }
<0>                 "{"                 { mkL LLBrace }
<0>                 "}"                 { mkL LRBrace }
<0>                 "@"                 { mkL LImpure }

-- Operators
<0>                 "+"                 { mkL LPlus }
<0>                 "-"                 { mkL LMinus }
<0>                 "/"                 { mkL LDivide }
<0>                 "%"                 { mkL LModulo }
<0>                 "*"                 { mkL LTimes }
<0>                 "<<"                { mkL LShiftLeft }
<0>                 ">>"                { mkL LShiftRight }
<0>                 ">>>"               { mkL LShiftRightSameSign }
<0>                 "&"                 { mkL LAndScrict }
<0>                 "&&"                { mkL LAndLazy }
<0>                 "|"                 { mkL LOrStrict }
<0>                 "||"                { mkL LOrLazy }
<0>                 "!"                 { mkL LNot }
<0>                 "^"                 { mkL LXor }
<0>                 "<"                 { mkL LLessThan }
<0>                 "<="                { mkL LLessThanOrEqual }
<0>                 ">"                 { mkL LGreaterThan }
<0>                 ">="                { mkL LGreaterThanOrEqual }
<0>                 "=>"                { mkL LImplies }
<0>                 "=="                { mkL LEqual }
<0>                 "!="                { mkL LNotEqual }

-- Significant whitespace
<0>                 @tabs               { mkL LTabs }
<0>                 \n                  { mkL LEoL }

{

data LexemeClass = LDocLine
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
                 | LPartSeparator
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
                                LDocLine            -> return (TDocLine            t p)
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
                                LPartSeparator      -> return (TPartSeparator      p)
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
data Token = TDocLine            { docLineContent :: String,    position :: AlexPosn } -- ^ Line of documentation
           | TInteger            { intVal :: Integer,           position :: AlexPosn } -- ^ An integral literal
           | TBool               { isTrue :: Bool,              position :: AlexPosn } -- ^ A boolean literal
           | TReal               { realVal :: Double,           position :: AlexPosn } -- ^ A real/floating-point literal
           | TChar               { charVal :: Char,             position :: AlexPosn } -- ^ A single character literal
           | TIf                 {                              position :: AlexPosn } -- ^ Keyword: @if@
           | TElse               {                              position :: AlexPosn } -- ^ Keyword: @else@
           | TWhile              {                              position :: AlexPosn } -- ^ Keyword: @while@
           | TRepeat             {                              position :: AlexPosn } -- ^ Keyword: @repeat@
           | TWith               {                              position :: AlexPosn } -- ^ Keyword: @with@
           | TSwitch             {                              position :: AlexPosn } -- ^ Keyword: @switch@
           | TFor                {                              position :: AlexPosn } -- ^ Keyword: @for@
           | TIdent              { identifierVal :: String,     position :: AlexPosn } -- ^ An identifier
           | TPartSeparator      {                              position :: AlexPosn } -- ^ @;@
           | TQueue              {                              position :: AlexPosn } -- ^ @<-@
           | TGoesTo             {                              position :: AlexPosn } -- ^ @->@
           | TGets               {                              position :: AlexPosn } -- ^ @=@
           | TLParenth           {                              position :: AlexPosn } -- ^ @(@
           | TRParenth           {                              position :: AlexPosn } -- ^ @)@
           | TLBracket           {                              position :: AlexPosn } -- ^ @[@
           | TRBracket           {                              position :: AlexPosn } -- ^ @]@
           | TLBrace             {                              position :: AlexPosn } -- ^ @{@
           | TRBrace             {                              position :: AlexPosn } -- ^ @}@
           | TImpure             {                              position :: AlexPosn } -- ^ @\@@
           | TPlus               {                              position :: AlexPosn } -- ^ @+@
           | TMinus              {                              position :: AlexPosn } -- ^ @-@
           | TDivide             {                              position :: AlexPosn } -- ^ @/@
           | TModulo             {                              position :: AlexPosn } -- ^ @%@
           | TTimes              {                              position :: AlexPosn } -- ^ @*@
           | TShiftLeft          {                              position :: AlexPosn } -- ^ @<<@
           | TShiftRight         {                              position :: AlexPosn } -- ^ @>>@
           | TShiftRightSameSign {                              position :: AlexPosn } -- ^ @>>>@
           | TAndScrict          {                              position :: AlexPosn } -- ^ @&@
           | TAndLazy            {                              position :: AlexPosn } -- ^ @&&@
           | TOrStrict           {                              position :: AlexPosn } -- ^ @|@
           | TOrLazy             {                              position :: AlexPosn } -- ^ @||@
           | TNot                {                              position :: AlexPosn } -- ^ @!@
           | TXor                {                              position :: AlexPosn } -- ^ @^@
           | TLessThan           {                              position :: AlexPosn } -- ^ @<@
           | TLessThanOrEqual    {                              position :: AlexPosn } -- ^ @<=@
           | TGreaterThan        {                              position :: AlexPosn } -- ^ @>@
           | TGreaterThanOrEqual {                              position :: AlexPosn } -- ^ @>=@
           | TImplies            {                              position :: AlexPosn } -- ^ @=>@
           | TEqual              {                              position :: AlexPosn } -- ^ @==@
           | TNotEqual           {                              position :: AlexPosn } -- ^ @!=@
           | TComma              {                              position :: AlexPosn } -- ^ @,@
           | TTabs               { numTabs :: Int,              position :: AlexPosn } -- ^ @\t@
           | TEoL                {                              position :: AlexPosn } -- ^ @\\n@
           | TEoF                                                                      -- ^ @\\0@
    deriving (Eq, Ord, Show)

-- | AlexPosn is ordered by the total number of characters read (its final field)
instance Ord AlexPosn where
    (AlexPn c1 _ _ ) < (AlexPn c2 _ _) = c1 < c2
    a <= b = (a < b) || (a == b)

}
