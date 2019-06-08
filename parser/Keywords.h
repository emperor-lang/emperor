#ifndef KEYWORDS_H
#define KEYWORDS_H

#include <stdio.h>
#include <string.h>

typedef enum purities 
{
	pure_v,
	impure_v
} purity_t;

typedef enum primitives
{
	int_v,
	real_v,
	boolean_v,
	char_v,
	string_v
} primitive_t;

typedef enum protections
{
	public_v,
	family_v,
	private_v
} protection_t;

purity_t makePurity(char* yytext);
primitive_t makePrimitive(char* yytext);
protection_t makeProtection(char* yytext);

#endif /* KEYWORDS_H */