%{
 
#include <stdio.h>
#include "emperor.tab.h"
#include "Primitives.h"

extern int currentLine;
extern int currentChar;

// %option bison-bridge
// %option bison-locations
%}
%option yylineno

%x docBlock
%x commentBlock
%x commentLine
%%
	int commentDepth = 0;
" "								;
"/**"							{ BEGIN(docBlock); }
<docBlock>{
	"/*"           				{ ++commentDepth; }
	"*"+"/"        				{ if (commentDepth > 0) --commentDepth;
									else BEGIN(INITIAL); }
	"*"+           				; /**/
	[^/*\n]+       				; /**/
	[/]            				; /**/
	\n             				; /* ADD THE DOCUMENTATION ACTUAL BODY BIT! */
}
"/*"							{ BEGIN(commentBlock); }
<commentBlock>{
	"/*"           				{ ++commentDepth; }
	"*"+"/"        				{ if (commentDepth > 0) --commentDepth;
									else BEGIN(INITIAL); }
	"*"+           				; /**/
	[^/*\n]+       				; /**/
	[/]            				; /**/
	\n             				; /**/
}
\/\/[^\n]*\n					;
\t*								{ return WHITESPACE; }
"\n"|"\r"|"\r\n"				{ return EOL; }
"@"								{ return AT; }
"."								{ return DOT; }
","								{ return COMMA; }
"#"								{ return HASH_SIGN; }
"_"								{ return IGNORE; }
"("								{ return OPEN_PARENTH; }
")"								{ return CLOSE_PARENTH; }
"["								{ return OPEN_SQUARE_BRACKET; }
"]"								{ return CLOSE_SQUARE_BRACKET; }
"->"							{ return RETURNS; }
"<-"							{ return GETS; }
"<=>"							{ return EQUIVALENT; }
">>"							{ return SHIFT_LEFT; }
"<<"							{ return SHIFT_RIGHT; }
">>>"							{ return SHIFT_RIGHT_SIGN; }
"&&"							{ return BOOLEAN_AND; }
"||"							{ return BOOLEAN_OR; }
"="								{ return VALUE_EQUAL; }
"!="							{ return VALUE_NOT_EQUAL; }
"=>"							{ return BOOLEAN_IMPLICATION; }
"<="							{ return LEQ; }
">="							{ return GEQ; }
"+"								{ return PLUS; }
"-"								{ return MINUS; }
"<"								{ return LT; }
">"								{ return GT; }
"!"								{ return BOOLEAN_NOT; }
"~"								{ return BITWISE_NOT; }
"&"								{ return BITWISE_AND; }
"|"								{ return BITWISE_OR; }
"*"								{ return MULTIPLY; }
"%" 							{ return MODULO; }
"/" 							{ return DIVIDE; }
"?"								{ return QUESTION_MARK; }
":"								{ return COLON; }
"void"							{ return VOID; }
-?[0-9]+ 	 					{ yylval.integer = makeInt(yytext); 			return NUMBER; }
-?[0-9]+\.[0-9]+ 				{ yylval.real = makeReal(yytext); 				return REAL; }
true|false						{ yylval.boolean = makeBool(yytext); 			return BOOLEAN_VALUE; }
pure|impure						{ yylval.purity = makePurity(yytext); 			return FUNCTION_PURITY; }
int|real|boolean|char|string	{ yylval.primitive = makePrimitive(yytext); 	return PRIMITIVE_TYPE; }
public|family|private			{ yylval.protection = makeProtection(yytext); 	return ACCESS_MODIFIER; }
\'(\\.|[^'\\])\'				{ yylval.character = makeChar(yytext); 			return CHARACTER; }
\"(\\.|[^"\\])*\"				{ yylval.string = makeString(yytext); 			return STRING; }
[A-Za-z£][A-Za-z0-9£]* 			{ yylval.name = strdup(yytext); 				return NAME; }
.								{ return yytext[0]; }
%%

// /**
//  * Precondition: the `binary` string consists only of the characters `0` and `1`.
// */
// int binaryToInt(const char* binary)
// {
// 	printf("%s\n", binary);
// 	int value = 0;
// 	int position = 0x1;
// 	int i;
// 	int len = strlen(binary);
// 	for(i = 0; i < 32; i++)
// 	{
// 		if (binary[i] == '1')
// 		{
// 			value |= position;
// 		}
// 		position <<= 1;
// 		if (i <= len)
// 		{
// 			break;
// 		}
// 	}
// 	return value;
// }

// int hexadecimalToInt(const char* hexadecimal);


// Keywords: BLOCKS:	if, else, while, for, foreach, repeat,
// Keywords: FUNCTIONS:	pure, impure
// Keywords: MODIFIERS:	public, private, protected
// Keywords: PACKAGES:	package	
