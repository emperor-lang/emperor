LEX = flex
LEXFLAGS = #-Cfr -8

YACC = bison
YACCFLAGS = -v -d

CC = gcc-8
CFLAGS = -fPIC -shared # -shared -fPIC

PARSER_OUTPUT_FILE := parser.so

.PHONY: all clean;

all: parser.o;

parser.o: emperor.tab.c emperor.yy.c AST.c Primitives.c Keywords.c Symbols.c
	$(CC) $(CFLAGS) -o $(PARSER_OUTPUT_FILE) AST.c Primitives.c Keywords.c Symbols.c emperor.tab.c emperor.yy.c

emperor.yy.c: emperor.lex
	$(LEX) $(LEXFLAGS) -o emperor.yy.c emperor.lex 

emperor.tab.c: emperor.yacc
	$(YACC) $(YACCFLAGS) -o emperor.tab.c emperor.yacc 

emperor.lex:;
emperor.yacc:;
AST.c:;
Primitives.c:;
Symbols.c:;

clean:
	-@ rm *.o		2>/dev/null	|| true
	-@ rm *.so		2>/dev/null	|| true
	-@ rm *.yy.c	2>/dev/null	|| true
	-@ rm *.tab.*	2>/dev/null	|| true
	-@ rm *.output	2>/dev/null	|| true 
	
