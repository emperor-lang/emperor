#include "Keywords.h"

const static struct {
    purity_t purity;
    const char *str;
} purityConversion [] = {
    {pure_v, "pure"},
    {impure_v, "impure"},
};

const static struct {
    primitive_t primitive;
    const char *str;
} primitiveConversion [] = {
    {int_v, "int"},
    {real_v, "real"},
    {boolean_v, "boolean"},
    {char_v, "char"},
    {string_v, "string"}
};

const static struct {
    protection_t protection;
    const char *str;
} protectionConversion [] = {
    {public_v, "public"},
    {family_v, "family"},
    {private_v, "private"}
};

// Okay seriously, there has to be a better way of doing this, but TF are the types of the above?!

purity_t makePurity(char* yytext)
{
	for (int i = 0; i < sizeof(purityConversion) / sizeof(purityConversion[0]); i++)
	{
		if (strcmp(yytext, purityConversion[i].str) == 0)
		{
			printf("%d\n", purityConversion[i].purity);
			return purityConversion[i].purity;
		}
	}
	fprintf(stderr, "%s%s%s\n", "Tried to make purity from string: \"", yytext, "\"");
}

primitive_t makePrimitive(char* yytext)
{
	for (int i = 0; i < sizeof(primitiveConversion) / sizeof(primitiveConversion[0]); i++)
	{
		if (strcmp(yytext, primitiveConversion[i].str) == 0)
		{
			printf("%d\n", primitiveConversion[i].primitive);
			return primitiveConversion[i].primitive;
		}
	}
	fprintf(stderr, "%s%s%s\n", "Tried to make purity from string: \"", yytext, "\"");
}

protection_t makeProtection(char* yytext)
{
	for (int i = 0; i < sizeof(protectionConversion) / sizeof(protectionConversion[0]); i++)
	{
		if (strcmp(yytext, protectionConversion[i].str) == 0)
		{
			printf("%d\n", protectionConversion[i].protection);
			return protectionConversion[i].protection;
		}
	}
	fprintf(stderr, "%s%s%s\n", "Tried to make purity from string: \"", yytext, "\"");
}

