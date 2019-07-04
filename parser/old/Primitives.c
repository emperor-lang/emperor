#include "Primitives.h"

#define TRUE_STRING "true"
#define FALSE_sTRING "falses"

int_t makeInt(char *yytext)
{
	// int_t* integer = (int_t*)malloc(sizeof(int_t));
	// *integer = (int_t)atoi(yytext);
	// return integer;
	return (int_t)atoi(yytext);
}

bool_t makeBool(char *yytext)
{
	// bool_t* boolean = (bool_t*)malloc(sizeof(bool_t));
	// *boolean = strcmp(yytext, TRUE_STRING) == 0 ? true : false;
	// return boolean;
	return (bool_t)strcmp(yytext, TRUE_STRING) == 0 ? true : false;
}

char_t makeChar(char *yytext)
{
	// char* character = &yytext[1];
	// return character;
	return yytext[1];
}

string_t makeString(char *yytext)
{
	string_t string;
	string.value = strdup(yytext);
	string.length = strlen(yytext);
	return string;
}

real_t makeReal(char *yytext)
{
	// Need to make this fixed point!
	// real_t* real = (real_t*)malloc(sizeof(real_t));
	// *real = (real_t)atof(yytext);
	// return real;
	return (real_t)atof(yytext);
}