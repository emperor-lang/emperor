#!/usr/bin/make
SHELL := /bin/bash

# CC = gcc-8
# CFLAGS := $(shell emperor-setup --cflags) # $(CFLAGS) -Wall -Os -I . -I /usr/include/python3.6m -g
# CLIBS := $(shell emperor-setup --libs)

# Apply debug options if specified
ifdef DEBUG
PARSER_DEBUG_FLAGS = -d
endif

OPEN := xdg-open

# Code generation commands
LEXER_GENERATOR := alex
LEXER_GENERATOR_FLAGS := -g
PARSER_GENERATOR := happy
PARSER_GENERATOR_FLAGS := -ga -m emperorParser
PATCH := patch
PATCHFLAGS := -F 0 -s
FORMATTER_FLAGS_VALIDATE := $(FORMATTER_FLAGS) --validate
LEXER_GENERATOR = alex
LEXER_GENERATOR_FLAGS = -g
PARSER_GENERATOR = happy
PARSER_GENERATOR_FLAGS = -ga $(PARSER_DEBUG_FLAGS) -m emperorParser
SOFT_LINK_COMMAND = [[ ! -f $@ ]] && ln -s $^ $@

# Code up-keep commands
LINTER := hlint
LINTER_FLAGS := -s
FORMATTER := hindent
FORMATTER_FLAGS := --tab-size 4 --line-length 120

COMPLETION_INSTALL_LOCATION = /usr/share/bash-completion/completions/emperor

.DEFAULT_GOAL := all

# All required source files (existent or otherwise)
SOURCE_FILES = $(shell find . -name '*.hs' | grep -v dist) ./Args.hs ./parser/EmperorLexer.hs ./parser/EmperorParser.hs

all: build ## Build everything
.PHONY: all

build: ./emperor
.PHONY: build

./emperor: ./dist/build/emperor/emperor
	@echo "[[ ! -f $@ ]] && ln -s $^ $@"
	$(shell [[ ! -f $@ ]] && ln -s $^ $@)
.DELETE_ON_ERROR: ./emperor

./dist/build/emperor/emperor: $(SOURCE_FILES)
	cabal build $(CABALFLAGS)

./parser/EmperorLexer.hs: ./parser/EmperorLexer.x ./parser/EmperorLexer.hs.patch
	$(LEXER_GENERATOR) $(LEXER_GENERATOR_FLAGS) $< -o $@
	$(PATCH) $(PATCHFLAGS) $@ $@.patch
.DELETE_ON_ERROR: ./parser/EmperorLexer.hs

./parser/EmperorParser.hs: ./parser/EmperorParser.y ./parser/EmperorParser.hs.patch
	$(PARSER_GENERATOR) $(PARSER_GENERATOR_FLAGS) -i./parser/EmperorParser.info $< -o $@
	$(PATCH) $(PATCHFLAGS) $@ $@.patch
.DELETE_ON_ERROR: ./parser/EmperorParser.hs

%.patch:;

./Args.hs: emperor.json
	arggen_haskell < $^ > $@

%.hs:;

./emperor.json:;

install: /usr/bin/emperor /usr/share/man/man1/emperor.1.gz $(COMPLETION_INSTALL_LOCATION);
.PHONY: install

/usr/bin/emperor: ./dist/build/emperor/emperor
	sudo install -m 755 $^ $@

man: ./dist/doc/man/emperor.1.gz;
.PHONY: man

/usr/share/man/man1/emperor.1.gz: ./dist/doc/man/emperor.1.gz
	sudo install -m 644 $^ $@

./dist/doc/man/emperor.1.gz: emperor.json
	mkdir -p ./dist/doc/man/ 2>/dev/null || true
	(mangen | gzip --best) < $^ > $@
.DELETE_ON_ERROR: ./dist/doc/man/emperor.1.gz

$(COMPLETION_INSTALL_LOCATION): ./emperor_completions.sh;
	sudo install -m 644 $^ $@

./emperor_completions.sh: ./emperor.json
	argcompgen < $< > $@
.DELETE_ON_ERROR: ./emperor_completions.sh

validate-format: $(shell find . -name '*.hs' | grep -v dist | grep -v Args.hs | grep -v parser/EmperorLexer.hs | grep -v parser/EmperorParser.hs)
	$(FORMATTER) $(FORMATTER_FLAGS_VALIDATE) $^
.PHONY: validate-format

format: $(shell find . -name '*.hs' | grep -v dist | grep -v Args.hs | grep -v parser/EmperorLexer.hs | grep -v parser/EmperorParser.hs)
	$(FORMATTER) $(FORMATTER_FLAGS) $^
.PHONY: format

lint: $(shell find . -name '*.hs' | grep -v dist | grep -v Args.hs | grep -v parser/EmperorLexer.hs | grep -v parser/EmperorParser.hs)
	$(LINTER) $(LINTER_FLAGS) $^
.PHONY: lint

doc: dist/doc/html/emperor/emperor/index.html ./dist/doc/man/emperor.1.gz
.PHONY: doc

open-doc: dist/doc/html/emperor/emperor/index.html
	$(OPEN) $<
.PHONY: open-doc

dist/doc/html/emperor/emperor/index.html: $(SOURCE_FILES)
	cabal haddock --executables

clean-installation:
	sudo $(RM) /usr/bin/emperor
	sudo $(RM) /usr/share/man/man1/emperor.1.gz
	sudo $(RM) /usr/share/bash-completion/completions/emperor 2>/dev/null || true
.PHONY: clean-installation

clean:
	cabal clean --verbose=0
	$(RM) cabal.config Args.hs *_completions.sh ./emperor ./parser/Emperor{Lexer,Parser,ParserData}.hs ./parser/EmperorParser.info $(shell find . -name '*.orig') $(shell find . -name '*.info') $(shell find . -name '*.hi') *.eh*
.PHONY: clean
