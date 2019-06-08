%{
	
	#include "emperor.tab.h"

	typedef void* yyscan_t;

	int currentLine = 1;
	int currentChar = 1;
	
	int parseFiles(int totalFiles, char** files);
	int yylex(void);
	void yyerror(FILE* fp, const char* s);

	extern int yylex_init(yyscan_t* scanner);
	extern int yyset_in(FILE* inputFile, yyscan_t* scanner);
	extern int yylex_destroy(yyscan_t* scanner);

	int parseFile(FILE* fp);
	extern struct yy_buffer_state* yy_scan_string(char* str);
	extern void yy_delete_buffer(struct yy_buffer_state* buffer);
%}
%language "C" 

%code requires { #include <stdio.h> } 
%code requires { #include <stdlib.h> } 
%code requires { #include <string.h> } 
%code requires { #include <unistd.h> } 
%code requires { #include <printf.h> } 
%code requires { #include "AST.h" } 
%code requires { #define STDIN_FLAG "-" } 
%code requires { int parseStd(void); } 
%code requires { int parseFiles(int totalFiles, char** files); }

%start program

%parse-param { FILE* fp }

%locations

// %define api.pure full
%define parse.error verbose
%define parse.lac full
%verbose

// %define api.value.type {YYSTYPE_t}
%code requires {#include "AST.h"}
%code requires {#include "Keywords.h"}
%code requires {#include "Primitives.h"}
%code requires {#include "Symbols.h"}

%union 
{
	int_t integer;
	real_t real;
	bool_t boolean;
	char_t character;
	string_t string;
	purity_t purity;
	primitive_t primitive;
	protection_t protection;
	name_t name;
	AstNode_t *AstNode;
}

// Misc
%token EOL
%token AT
%token DOT
%token COMMA
%token HASH_SIGN
%token IGNORE
// Brackets
%token OPEN_PARENTH
%token CLOSE_PARENTH
%token OPEN_SQUARE_BRACKET
%token CLOSE_SQUARE_BRACKET
%token OPEN_ANGLE
%token CLOSE_ANGLE
// OPERATORS
%token EQUIVALENT
%token SHIFT_LEFT
%token SHIFT_RIGHT
%token SHIFT_RIGHT_SIGN
%token BOOLEAN_AND
%token BOOLEAN_OR
%token BOOLEAN_NOT
%token VALUE_EQUAL
%token VALUE_NOT_EQUAL
%token BOOLEAN_IMPLICATION
%token LEQ
%token GEQ
%token PLUS
%token MINUS
%token LT
%token GT
%token BITWISE_AND
%token BITWISE_OR
%token BITWISE_NOT
%token MULTIPLY
%token MODULO
%token DIVIDE
// Other
%token RETURNS
%token GETS
%token QUESTION_MARK
%token COLON
%token VOID
%token <integer> NUMBER
%token <real> REAL
%token <boolean> BOOLEAN_VALUE
%token <purity> FUNCTION_PURITY
%token <primitive> PRIMITIVE_TYPE
%token <protection> ACCESS_MODIFIER
%token <character> CHARACTER
%token <string> STRING
%token <name> NAME
%token OPEN_COMMENT
%token CLOSE_COMMENT
%token WHITESPACE

%left COMMA
%left QUESTION_MARK COLON
%left EQUIVALENT
%left SHIFT_RIGHT_SIGN
%left SHIFT_RIGHT SHIFT_LEFT
%left BOOLEAN_AND BOOLEAN_OR BOOLEAN_IMPLICATION
%left VALUE_EQUAL VALUE_NOT_EQUAL
%left LEQ GEQ LT GT
%left PLUS MINUS
%left BITWISE_AND BITWISE_OR
%left MULTIPLY DIVIDE MODULO
%right BITWISE_NOT BOOLEAN_NOT

// Terminals with Values

%type<AstNode> program
%type<AstNode> line
%type<AstNode> startLineWhiteSpace
%type<AstNode> lineContents
%type<AstNode> functionalLine
%type<AstNode> declaration
%type<AstNode> type_list_non_zero
%type<AstNode> functionDeclarationLine
%type<AstNode> type
%type<AstNode> type_list
%type<AstNode> functionType
%type<AstNode> parameters
%type<AstNode> parameters_non_zero
%type<AstNode> parameter
%type<AstNode> returnType
%type<AstNode> assignment
%type<AstNode> declarationWithAssignment
%type<AstNode> expression
%type<AstNode> value
%type<AstNode> value_list
%type<AstNode> functionCall
%type<AstNode> impureFunctionCall
%type<AstNode> pureFunctionCall
%type<AstNode> arguments
%type<AstNode> args_non_zero
%type<AstNode> argument
%type<AstNode> variable
%type<AstNode> variable_list_with_ignores
%type<AstNode> variable_or_ignore

%%									 /* beginning of rules section */
program: line						{ $$ = $1; }
	   | line EOL program			{ $$ = makeJoiningNode(program_v, 2, $1, $3); }
	   ;
line: startLineWhiteSpace lineContents	{ $$ = $2; }
	;
startLineWhiteSpace:								{ $$ = NULL; }
				   | WHITESPACE startLineWhiteSpace	{ $$ = NULL; } 
				   ;
lineContents:								{ $$ = NULL; }
			| functionalLine				{ $$ = $1; }
			| functionDeclarationLine		{ $$ = $1; }
			;
functionalLine: impureFunctionCall			{ $$ = $1; }
			  | declaration					{ $$ = $1; }
			  | declarationWithAssignment	{ $$ = $1; }
			  | assignment					{ $$ = $1; }
			  ;
declaration: type variable	{ $$ = makeJoiningNode(declaration_v, 2, $1, $2); }
		   ;
type_list_non_zero: type							{ $$ = $1; }
				  | type COMMA type_list_non_zero	{ $$ = makeJoiningNode(type_list_non_zero_v, 2, $1, $3); }
				  ;
functionDeclarationLine: FUNCTION_PURITY NAME OPEN_PARENTH parameters CLOSE_PARENTH RETURNS returnType	{ $$ = makeJoiningNode(functionDeclarationLine_v, 4, makeLeaf(FUNCTION_PURITY_v, (NodeValue_t){.purity_v = $1}), makeLeaf(NAME_v, (NodeValue_t){.name = $2}), $4, $7); }
					   ;
type: PRIMITIVE_TYPE									{ $$ = makeLeaf(type_v, (NodeValue_t){.primitive_v = $1}); }
	| type OPEN_ANGLE type_list CLOSE_ANGLE				{ $$ = makeNode(type_v, (NodeValue_t){.val = GENERIC_TYPE}, 2, $1, $3); }
	| OPEN_PARENTH type_list CLOSE_PARENTH				{ $$ = makeNode(type_v, (NodeValue_t){.val = TUPLE_TYPE}, 1, $2); }
	| OPEN_SQUARE_BRACKET type CLOSE_SQUARE_BRACKET		{ $$ = makeNode(type_v, (NodeValue_t){.val = LIST_TYPE}, 1, $2); }
	| functionType										{ $$ = $1; }
	| NAME												{ $$ = makeLeaf(type_v, (NodeValue_t){.name = $1}); }
	;
type_list: 						{ $$ = NULL; }
		 | type_list_non_zero	{ $$ = $1; }
		 ;
functionType: HASH_SIGN OPEN_PARENTH type_list CLOSE_PARENTH RETURNS type { $$ = makeJoiningNode(functionType_v, 2, $3, $6); }
			;
parameters: 						{ $$ = NULL; }
		  | parameters_non_zero		{ $$ = $1; }
		  ;
parameters_non_zero: parameter								{ $$ = $1; }
				   | parameter COMMA parameters_non_zero	{ $$ = makeJoiningNode(parameters_non_zero_v, 2, $1, $3); }
				   ;
parameter: type NAME	{ $$ = makeJoiningNode(parameter_v, 2, $1, makeLeaf(NAME_v, (NodeValue_t){.name = $2})); }
		 ;
returnType: type	{ $$ = $1; }
		  | VOID 	{ $$ = makeLeaf(returnType_v, (NodeValue_t){.val = VOID}); }
		  ;

assignment: variable GETS expression						{ $$ = makeJoiningNode(assignment_v, 2, $1, $3); }
		  ;
declarationWithAssignment: type variable GETS expression	{ $$ = makeJoiningNode(declarationWithAssignment_v, 3, $1, $2, $4); }
						 ;

expression: value 													{ $$ = $1; }
		  | functionCall											{ $$ = $1; }
		  | MINUS expression										{ $$ = makeNode(expression_v, (NodeValue_t){.val = MINUS}, 					1, $2); }
		  | BITWISE_NOT expression									{ $$ = makeNode(expression_v, (NodeValue_t){.val = BITWISE_NOT}, 			1, $2); }
		  | BOOLEAN_NOT expression									{ $$ = makeNode(expression_v, (NodeValue_t){.val = BOOLEAN_NOT}, 			1, $2); }
		  | expression MINUS expression								{ $$ = makeNode(expression_v, (NodeValue_t){.val = MINUS},					2, $1, $3); }
		  | expression EQUIVALENT expression						{ $$ = makeNode(expression_v, (NodeValue_t){.val = EQUIVALENT},				2, $1, $3); }
		  | expression SHIFT_LEFT expression						{ $$ = makeNode(expression_v, (NodeValue_t){.val = SHIFT_LEFT},				2, $1, $3); }
		  | expression SHIFT_RIGHT expression						{ $$ = makeNode(expression_v, (NodeValue_t){.val = SHIFT_RIGHT},			2, $1, $3); }
		  | expression SHIFT_RIGHT_SIGN expression					{ $$ = makeNode(expression_v, (NodeValue_t){.val = SHIFT_RIGHT_SIGN},		2, $1, $3); }
		  | expression BOOLEAN_AND expression						{ $$ = makeNode(expression_v, (NodeValue_t){.val = BOOLEAN_AND},			2, $1, $3); }
		  | expression BOOLEAN_OR expression						{ $$ = makeNode(expression_v, (NodeValue_t){.val = BOOLEAN_OR},				2, $1, $3); }
		  | expression VALUE_EQUAL expression						{ $$ = makeNode(expression_v, (NodeValue_t){.val = VALUE_EQUAL},			2, $1, $3); }
		  | expression VALUE_NOT_EQUAL expression					{ $$ = makeNode(expression_v, (NodeValue_t){.val = VALUE_NOT_EQUAL},		2, $1, $3); }
		  | expression BOOLEAN_IMPLICATION expression				{ $$ = makeNode(expression_v, (NodeValue_t){.val = BOOLEAN_IMPLICATION},	2, $1, $3); }
		  | expression LEQ expression								{ $$ = makeNode(expression_v, (NodeValue_t){.val = LEQ},					2, $1, $3); }
		  | expression GEQ expression								{ $$ = makeNode(expression_v, (NodeValue_t){.val = GEQ},					2, $1, $3); }
		  | expression PLUS expression								{ $$ = makeNode(expression_v, (NodeValue_t){.val = PLUS},					2, $1, $3); }
		  | expression MINUS expression								{ $$ = makeNode(expression_v, (NodeValue_t){.val = MINUS},					2, $1, $3); }
		  | expression LT expression								{ $$ = makeNode(expression_v, (NodeValue_t){.val = LT},						2, $1, $3); }
		  | expression GT expression								{ $$ = makeNode(expression_v, (NodeValue_t){.val = GT},						2, $1, $3); }
		  | expression BITWISE_AND expression						{ $$ = makeNode(expression_v, (NodeValue_t){.val = BITWISE_AND},			2, $1, $3); }
		  | expression BITWISE_OR expression						{ $$ = makeNode(expression_v, (NodeValue_t){.val = BITWISE_OR},				2, $1, $3); }
		  | expression MULTIPLY expression							{ $$ = makeNode(expression_v, (NodeValue_t){.val = MULTIPLY},				2, $1, $3); }
		  | expression MODULO expression							{ $$ = makeNode(expression_v, (NodeValue_t){.val = MODULO},					2, $1, $3); }
		  | expression DIVIDE expression							{ $$ = makeNode(expression_v, (NodeValue_t){.val = DIVIDE},					2, $1, $3); }
		  | expression QUESTION_MARK expression COLON expression	{ $$ = makeNode(expression_v, (NodeValue_t){.val = QUESTION_MARK},			3, $1, $3, $5); }
		  ;
value: NUMBER			{ $$ = makeLeaf(value_v, (NodeValue_t){.int_v = $1}); }
	 | REAL				{ $$ = makeLeaf(value_v, (NodeValue_t){.real_v = $1}); }
	 | BOOLEAN_VALUE	{ $$ = makeLeaf(value_v, (NodeValue_t){.bool_v = $1}); }
	 | CHARACTER		{ $$ = makeLeaf(value_v, (NodeValue_t){.char_v = $1}); }
	 | STRING			{ $$ = makeLeaf(value_v, (NodeValue_t){.string_v = $1}); }
	 | variable			{ $$ = $1; }
	 | OPEN_PARENTH value_list CLOSE_PARENTH { $$ = makeNode(value_v, (NodeValue_t){.val = TUPLE_VALUE}, 1, $2); }
	 ;
value_list: value					{ $$ = $1; }
	      | value COMMA value_list	{ $$ = makeJoiningNode(value_list_v, 2, $1, $3); }
		  ;
functionCall: pureFunctionCall		{ $$ = $1; }
			| impureFunctionCall	{ $$ = $1; }
			;
impureFunctionCall: AT pureFunctionCall { $$ = makeJoiningNode(impureFunctionCall_v, 1, $2); }
				  ;
pureFunctionCall: NAME OPEN_PARENTH arguments CLOSE_PARENTH	{ $$ = makeJoiningNode(pureFunctionCall_v, 2, $1, $3); }
				;

arguments: 					{ $$ = NULL; }
		 | args_non_zero	{ $$ = $1; }
		 ;
args_non_zero: argument							{ $$ = $1; }
			 | argument COMMA args_non_zero		{ $$ = makeJoiningNode(args_non_zero_v, 2, $1, $3); }
			 ;
argument: expression	{ $$ = $1; }
		;
variable: NAME 																{ $$ = makeLeaf(variable_v, (NodeValue_t){.name = $1}); }
		| variable OPEN_SQUARE_BRACKET expression CLOSE_SQUARE_BRACKET		{ $$ = makeJoiningNode(variable_v, 2, $1, $3); }
		| OPEN_PARENTH variable_list_with_ignores CLOSE_PARENTH				{ $$ = makeJoiningNode(variable_v, 1, $2); }
		;
variable_list_with_ignores: variable_or_ignore									{ $$ = makeJoiningNode(variable_list_with_ignores_v, 1, $1); }
						  | variable_or_ignore COMMA variable_list_with_ignores	{ $$ = makeJoiningNode(variable_list_with_ignores_v, 2, $1, $3); }
						  ;
variable_or_ignore: variable	{ $$ = makeJoiningNode(variable_or_ignore_v, 1, $1); }
				  | IGNORE		{ $$ = makeLeaf(variable_or_ignore_v, (NodeValue_t){.val = IGNORE_v}); }
				  ;
%%

/*
 * Precondition: array has a non-negative number of elements
*/
int parseFiles(int totalFiles, char** files)
{
	FILE* fps[totalFiles];

	int returnCode = 0;
	for (int i = 0; i < totalFiles; i++)
	{
		// Resolve file pointer
		if (strcmp(files[i], STDIN_FLAG) != 0)
		{
			fps[i] = fopen(files[i], "r");
			if(fps[i] == NULL)
			{
				fprintf(stderr, "%s%s%s\n", "File \"", files[i], "\" does not exist");
				returnCode = 0x1; 
			}
			else if (access(files[i], F_OK) != 0)
			{
				fprintf(stderr, "Could not open file '%s'\n", files[i]);
				returnCode = 0x1;
			}
		}
		else
		{
			fps[i] = stdin;
		}
	}
	if(returnCode != 0)
	{
		fprintf(stderr, "%s\n", "Compilation ended early due to missing input files.");
		return returnCode;
	}

	for (int i = 0; i < totalFiles; i++)
	{
		returnCode = parseFile(fps[i]);

		if (fps[i] != stdin)
		{
			fclose(fps[i]);
		}

		if (returnCode != 0)
		{
			printf("%s %s\n", "Compilation failed while processing file:", files[i]);
			break;
		}
	}
	return returnCode;
}

int parseFile(FILE* fp)
{
	// Open the input file (something on disk, or stdin)
	yyscan_t scanner = NULL;	
	// yylex_init(&scanner);
	yyset_in(fp, scanner);
	int returnValue = yyparse(scanner);
	yylex_destroy(scanner);

	return returnValue;
}

int parseStd(void)
{
	char** arguments = (char**)malloc(1 * sizeof(char*));
	arguments[0] = STDIN_FLAG;
	printf("%s\n", "Reading input from stdin");
	return parseFiles(1, arguments);
}

extern int parserMain(int argc, char** argv)
{
	if (argc <= 1)
	{
		return parseStd();
	}
	else
	{
		char** arguments = (char**)malloc(argc * sizeof(char*));
		for(int i = 0; i < argc - 1; i++)
		{
			arguments[i] = argv[i + 1];
		}
		int returnCode = parseFiles(argc - 1, arguments);
		free(arguments);
		return returnCode;
	}
}

void yyerror(FILE* fp, const char* s)
{
	// Fix bison-bridge and bison-locations for yylloc here!
	fprintf(stderr, "%s %s\n", "!",s);
}

int yywrap(void)
{
	return 1;
}
