{
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
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
module Parser.EmperorLexer (Alex, AlexPosn(..), Token(..), lexWrap, alexError, runAlex) where

import Data.Aeson (FromJSON, Value(Object), ToJSON, (.:), (.=), object, parseJSON, toJSON)

}

%wrapper "monad"

$alpha = [A-Za-z]
$num = [0-9]
$alphaNum = [$alpha$num]
@newline = \r\n | \r | \n

@newline = \r\n | \r | \n
@tabs = \t+
@spaces = \ +

@whitespace = @tabs | @spaces

@ident = $alpha $alphaNum*
@int = $num+
@real = $num+ \. $num+
@bool = (true) | (false)
@char = \'$alphaNum\'
$stringchar = [^\n"]
@string = \"$stringchar*\"

@partSeparator = (";" @newline*) | (@newline+)
@blockStarter = ":" @newline*
@blockSeparator = "#" @newline*

@partSeparator = (";" @newline?) | (@newline)
@blockStarter = ":" @newline?
@blockSeparator = "#" @newline?

@docStart = @tabs? "/*" \n?
@docEnd = @tabs? "*/" \n?
@docLineStart = @tabs? " *" (@spaces | @tabs)?

@lineComment = \/\/ .* @newline @tabs?
@ignoredWhitespace = \\ @newline @whitespace*

:-

-- Things to ignore
@spaces             ;
@tabs+              ;
@lineComment        ;
@ignoredWhitespace  ;

-- -- Documentation
-- <0>                 @docStart           { begin docs }
-- <docs>              @docLineStart       { begin docLineContents }
-- <docLineContents>   .*                  { mkL LDocLine }
-- <docLineContents>   \n                  { begin docs }
-- <docLineContents>   @docEnd             { begin 0 }
-- <docs>              @docEnd             { begin 0 }
-- -- <0>                 @docLine            { mkL LDocLine }

-- Values
@int                { mkL LInteger }
@bool               { mkL LBool }
@real               { mkL LReal }
@char               { mkL LChar }
@string             { mkL LString }

-- Types
"int"               { mkL LIntT }
"bool"              { mkL LBoolT }
"real"              { mkL LRealT }
"char"              { mkL LCharT }
"()"                { mkL LUnit }
"Any"               { mkL LAnyT }
"string"            { mkL LStringT }

-- Keywords
"_"                 { mkL LIDC }
"return"            { mkL LReturn }
"if"                { mkL LIf }
"else"              { mkL LElse }
"while"             { mkL LWhile }
"repeat"            { mkL LRepeat }
"with"              { mkL LWith }
"switch"            { mkL LSwitch }
"for"               { mkL LFor }
"import"            { mkL LImport }
"module"            { mkL LModule }
"<:"                { mkL LIsSubType }
"<~"                { mkL LIsImplementeBy }
"::"                { mkL LIsType }
"class"             { mkL LClass }
"component"         { mkL LComponent }

-- Identifiers
@ident              { mkL LIdent }

-- Things to ignore
@lineComment        ;
@spaces             ;
@ignoredWhitespace  ;
@tabs               ; -- { mkL LTabs }

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
@partSeparator      { mkL LPartSeparator }
@blockSeparator     { mkL LBlockSeparator }
@blockStarter       { mkL LColon }

-- Operators
"+"                 { mkL LPlus }
"-"                 { mkL LMinus }
"/"                 { mkL LDivide }
"%"                 { mkL LModulo }
"*"                 { mkL LTimes }
"<<"                { mkL LShiftLeft }
">>"                { mkL LShiftRight }
">>>"               { mkL LShiftRightSameSign }
"&"                 { mkL LAndScrict }
"&&"                { mkL LAndLazy }
"|"                 { mkL LOrStrict }
"||"                { mkL LOrLazy }
"!"                 { mkL LNot }
"^"                 { mkL LXor }
"<"                 { mkL LLessThan }
"<="                { mkL LLessThanOrEqual }
">"                 { mkL LGreaterThan }
">="                { mkL LGreaterThanOrEqual }
"=>"                { mkL LImplies }
"=="                { mkL LEqual }
"!="                { mkL LNotEqual }

-- Whitespace
\n                  ; -- { mkL LEoL }

{

data LexemeClass = LDocLine
                 | LInteger
                 | LBool
                 | LReal
                 | LChar
                 | LString
                 | LIf
                 | LElse
                 | LWhile
                 | LRepeat
                 | LWith
                 | LSwitch
                 | LFor
                 | LImport
                 | LModule
                 | LIdent
                 | LBlockSeparator
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
                 | LIntT
                 | LBoolT
                 | LRealT
                 | LCharT
                 | LStringT
                 | LUnit
                 | LAnyT
                 | LColon
                 | LIsSubType
                 | LIsImplementeBy
                 | LIsType
                 | LClass
                 | LComponent
                 | LIDC
                 | LReturn
                --  | LEoL
    deriving (Eq, Show)

mkL :: LexemeClass -> AlexInput -> Int -> Alex Token
mkL c (p, _, _, str) len = let t = take len str in
                            case c of
                                LDocLine            -> return (TDocLine            t p)
                                LInteger            -> return (TInteger            ((read t) :: Integer) p)
                                LBool               -> return (TBool               (if t == "true" then True else False) p)
                                LReal               -> return (TReal               ((read t) :: Double) p)
                                LChar               -> return (TChar               (t !! 1) p)
                                LString             -> return (TString             (init $ tail t) p)
                                LIf                 -> return (TIf                 p)
                                LElse               -> return (TElse               p)
                                LWhile              -> return (TWhile              p)
                                LRepeat             -> return (TRepeat             p)
                                LWith               -> return (TWith               p)
                                LSwitch             -> return (TSwitch             p)
                                LFor                -> return (TFor                p)
                                LImport             -> return (TImport             p)
                                LModule             -> return (TModule             p)
                                LIdent              -> return (TIdent              t p)
                                LPartSeparator      -> return (TPartSeparator      p)
                                LBlockSeparator     -> return (TBlockSeparator     p)
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
                                LIntT               -> return (TIntT               p)
                                LBoolT              -> return (TBoolT              p)
                                LRealT              -> return (TRealT              p)
                                LCharT              -> return (TCharT              p)
                                LStringT            -> return (TStringT            p)
                                LUnit               -> return (TUnit               p)
                                LAnyT               -> return (TAnyT               p)
                                LColon              -> return (TColon              p)
                                LIsSubType          -> return (TIsSubType          p)
                                LIsImplementeBy     -> return (TIsImplementeBy     p)
                                LIsType             -> return (TIsType             p)
                                LClass              -> return (TClass              p)
                                LComponent          -> return (TComponent          p )
                                LIDC                -> return (TIDC                p )
                                LReturn             -> return (TReturn             p )
                                -- LEoL                -> return (TEoL                p)

alexEOF :: Alex Token
alexEOF = return TEoF

-- | Wrapper function for the lexer---allows the monadic lexer to be used with
-- a monadic parser
lexWrap :: (Token -> Alex a) -> Alex a
lexWrap = (alexMonadScan >>=)

-- | Type to represent tokens in the output stream
data Token = TDocAssignmentLine  {                          position :: AlexPosn } -- ^ Documentation field line
           | TDocLine            { content :: String,       position :: AlexPosn } -- ^ Generic line of documentation
           | TInteger            { intVal :: Integer,       position :: AlexPosn } -- ^ An integral literal
           | TBool               { isTrue :: Bool,          position :: AlexPosn } -- ^ A boolean literal
           | TReal               { realVal :: Double,       position :: AlexPosn } -- ^ A real/floating-point literal
           | TChar               { charVal :: Char,         position :: AlexPosn } -- ^ A single character literal
           | TString             { stringVal :: String,     position :: AlexPosn } -- ^ A single character literal
           | TIf                 {                          position :: AlexPosn } -- ^ Keyword: @if@
           | TElse               {                          position :: AlexPosn } -- ^ Keyword: @else@
           | TWhile              {                          position :: AlexPosn } -- ^ Keyword: @while@
           | TRepeat             {                          position :: AlexPosn } -- ^ Keyword: @repeat@
           | TWith               {                          position :: AlexPosn } -- ^ Keyword: @with@
           | TSwitch             {                          position :: AlexPosn } -- ^ Keyword: @switch@
           | TFor                {                          position :: AlexPosn } -- ^ Keyword: @for@
           | TImport             {                          position :: AlexPosn } -- ^ Keyword: @import@
           | TModule             {                          position :: AlexPosn } -- ^ Keyword: @module@
           | TIdent              { identifierVal :: String, position :: AlexPosn } -- ^ An identifier
           | TPartSeparator      {                          position :: AlexPosn } -- ^ @;@
           | TBlockSeparator     {                          position :: AlexPosn } -- ^ @#@
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
           | TIntT               {                          position :: AlexPosn } -- ^ @int@
           | TBoolT              {                          position :: AlexPosn } -- ^ @bool@
           | TRealT              {                          position :: AlexPosn } -- ^ @real@
           | TCharT              {                          position :: AlexPosn } -- ^ @char@
           | TStringT            {                          position :: AlexPosn } -- ^ @char@
           | TUnit               {                          position :: AlexPosn } -- ^ @()@
           | TAnyT               {                          position :: AlexPosn } -- ^ @AnyT@
           | TColon              {                          position :: AlexPosn } -- ^ @AnyT@
           | TIsSubType          {                          position :: AlexPosn } -- ^ @<:@
           | TIsImplementeBy     {                          position :: AlexPosn } -- ^ @<~@
           | TIsType             {                          position :: AlexPosn } -- ^ @::@
           | TClass              {                          position :: AlexPosn } -- ^ @class@
           | TComponent          {                          position :: AlexPosn } -- ^ @component@
           | TIDC                {                          position :: AlexPosn } -- ^ @_@
           | TReturn             {                          position :: AlexPosn } -- ^ @_@
        --    | TEoL                {                          position :: AlexPosn } -- ^ @\\n@
           | TEoF                                                                  -- ^ @\\0@
    deriving (Eq, Ord)

instance Show Token where
    show (TDocAssignmentLine   _) = "TDocAssignmentLine"
    show (TDocLine           c _) = "// " ++ c
    show (TInteger           i _) = show i
    show (TBool              b _) = show b
    show (TReal              r _) = show r
    show (TChar              c _) = show c
    show (TString            s _) = show s
    show (TIf                  _) = "if"
    show (TElse                _) = "else"
    show (TWhile               _) = "while"
    show (TRepeat              _) = "repeat"
    show (TWith                _) = "with"
    show (TSwitch              _) = "switch"
    show (TFor                 _) = "for"
    show (TImport              _) = "import"
    show (TModule              _) = "module"
    show (TIdent             s _) = show s
    show (TPartSeparator       _) = ";/\\n"
    show (TBlockSeparator      _) = "#"
    show (TQueue               _) = "<-"
    show (TGoesTo              _) = "->"
    show (TGets                _) = "="
    show (TLParenth            _) = "("
    show (TRParenth            _) = ")"
    show (TLBracket            _) = "["
    show (TRBracket            _) = "]"
    show (TLBrace              _) = "{"
    show (TRBrace              _) = "}"
    show (TImpure              _) = "@"
    show (TPlus                _) = "+"
    show (TMinus               _) = "-"
    show (TDivide              _) = "/"
    show (TModulo              _) = "%"
    show (TTimes               _) = "*"
    show (TShiftLeft           _) = "<<"
    show (TShiftRight          _) = ">>"
    show (TShiftRightSameSign  _) = ">>>"
    show (TAndScrict           _) = "&"
    show (TAndLazy             _) = "&&"
    show (TOrStrict            _) = "|"
    show (TOrLazy              _) = "||"
    show (TNot                 _) = "!"
    show (TXor                 _) = "^"
    show (TLessThan            _) = "<"
    show (TLessThanOrEqual     _) = "<="
    show (TGreaterThan         _) = ">"
    show (TGreaterThanOrEqual  _) = ">="
    show (TImplies             _) = "=>"
    show (TEqual               _) = "=="
    show (TNotEqual            _) = "!="
    show (TComma               _) = ","
    show (TIntT                _) = "int"
    show (TBoolT               _) = "bool"
    show (TRealT               _) = "real"
    show (TCharT               _) = "char"
    show (TStringT               _) = "string"
    show (TUnit                _) = "Unit"
    show (TAnyT                _) = "Any"
    show (TColon               _) = ":"
    show (TIsSubType           _) = "<:"
    show (TIsImplementeBy      _) = "<~"
    show (TIsType              _) = "::"
    show (TClass               _) = "class"
    show (TComponent           _) = "component"
    show (TIDC                 _) = "_"
    show (TReturn              _) = "return"
    show TEoF                     = "EoF"

-- | AlexPosn is ordered by the total number of characters read (its final field)
instance Ord AlexPosn where
    (AlexPn c1 _ _ ) < (AlexPn c2 _ _) = c1 < c2
    a <= b = (a < b) || (a == b)

instance ToJSON AlexPosn where
    toJSON (AlexPn a b c) = object ["total" .= a, "line" .= b, "char" .= c]

instance FromJSON AlexPosn where
    parseJSON (Object v) = AlexPn <$> v .: "total" <*> v .: "line" <*> v .: "char"
    parseJSON _ = fail "Expected object when parsing position datum"

}
