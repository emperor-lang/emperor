#ifndef PRIMITIVES_H
#define PRIMITIVES_H

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef double real_t;
typedef bool bool_t;
typedef char char_t;
typedef int int_t;
typedef struct string
{
	const char *value;
	int length;
} string_t;
typedef struct reference
{
	int type;
	void* val;
} reference_t;

int_t makeInt(char *yytext);
bool_t makeBool(char *yytext);
char_t makeChar(char *yytext);
string_t makeString(char *yytext);
real_t makeReal(char *yytext);
#endif /* PRIMITIVES_H */