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
PATCHFLAGS := -F 1 -s # One line of fuzz is permitted as one lexer patch context catches an Alex address table
FORMATTER_FLAGS_VALIDATE := $(FORMATTER_FLAGS) --validate
LEXER_GENERATOR := alex
LEXER_GENERATOR_FLAGS := -g
PARSER_GENERATOR := happy
PARSER_GENERATOR_FLAGS := -ga $(PARSER_DEBUG_FLAGS) -m emperorParser
SOFT_LINK_COMMAND = [[ ! -f $@ ]] && ln -s $^ $@

# Code up-keep commands
LINTER := hlint
LINTER_FLAGS := -s
FORMATTER := hindent
FORMATTER_FLAGS := --tab-size 4 --line-length 120

HEADER_INSTALL_DIRECTORY = $(shell emperor-setup --language-header-location)
DEFAULT_HEADERS = $(shell find ./IncludedHeaders/ -type f | grep .h | sed "s/\.\/IncludedHeaders\///" | sed "s/^/$(shell emperor-setup --language-header-location | sed 's/\//\\\\\//g')/")
COMPLETION_INSTALL_LOCATION = /usr/share/bash-completion/completions/emperor

.DEFAULT_GOAL := all

# All required source files (existent or otherwise)
SOURCE_FILES = $(shell find . -name '*.hs' | grep -v dist) ./Args.hs ./Parser/EmperorLexer.hs ./Parser/EmperorParser.hs

all: build ## Build everything
.PHONY: all

build: ./emperor ## Build everything, explicitly
.PHONY: build

./emperor: ./dist-newstyle/build/x86_64-linux/ghc-8.6.5/emperor-0.1.0.0/x/emperor/build/emperor/emperor
	@echo "[[ ! -f $@ ]] && ln -sf $^ $@"
	$(shell [[ ! -f $@ ]] && ln -sf $^ $@)
.DELETE_ON_ERROR: ./emperor

./dist/build/emperor/emperor: $(SOURCE_FILES)
	cabal build $(CABALFLAGS)

./Parser/EmperorLexer.hs: ./Parser/EmperorLexer.x ./Parser/EmperorLexer.hs.patch
	$(LEXER_GENERATOR) $(LEXER_GENERATOR_FLAGS) $< -o $@
	$(PATCH) $(PATCHFLAGS) $@ $@.patch
.DELETE_ON_ERROR: ./Parser/EmperorLexer.hs

./Parser/EmperorParser.hs: ./Parser/EmperorParser.y ./Parser/EmperorParser.hs.patch
	$(PARSER_GENERATOR) $(PARSER_GENERATOR_FLAGS) -i./Parser/EmperorParser.info $< -o $@
	$(PATCH) $(PATCHFLAGS) $@ $@.patch
.DELETE_ON_ERROR: ./Parser/EmperorParser.hs

%.x:;
%.y:;
%.patch:;

./Args.hs: emperor.json
	arggen_haskell < $^ > $@

%.hs:;

./emperor.json:;

install: /usr/bin/emperor /usr/share/man/man1/emperor.1.gz $(COMPLETION_INSTALL_LOCATION) $(DEFAULT_HEADERS) ## Install binaries, libraries and documentation
.PHONY: install

$(HEADER_INSTALL_DIRECTORY)%.h: IncludedHeaders/%.h $(HEADER_INSTALL_DIRECTORY)
	sudo install -m 644 $< $@

$(HEADER_INSTALL_DIRECTORY):
	sudo mkdir -p $(HEADER_INSTALL_DIRECTORY)

$(HEADER_INSTALL_DIRECTORY)banned/%.h: IncludedHeaders/banned/%.h $(HEADER_INSTALL_DIRECTORY)banned/
	sudo install -m 644 $< $@

$(HEADER_INSTALL_DIRECTORY)banned/:
	sudo mkdir -p $(HEADER_INSTALL_DIRECTORY)banned/

/usr/bin/emperor: ./dist/build/emperor/emperor
	sudo install -m 755 $^ $@

man: ./dist/doc/man/emperor.1.gz; ## Make the man page
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

validate-format: $(shell find . -name '*.hs' | grep -v dist | grep -v Args.hs | grep -v Parser/EmperorLexer.hs | grep -v Parser/EmperorParser.hs)
	$(FORMATTER) $(FORMATTER_FLAGS_VALIDATE) $^
.PHONY: validate-format

format: $(shell find . -name '*.hs' | grep -v dist | grep -v Args.hs | grep -v Parser/EmperorLexer.hs | grep -v Parser/EmperorParser.hs) ## Run the formatter on all non-generated source files
	$(FORMATTER) $(FORMATTER_FLAGS) $^
.PHONY: format

lint: $(shell find . -name '*.hs' | grep -v dist | grep -v Args.hs | grep -v Parser/EmperorLexer.hs | grep -v Parser/EmperorParser.hs) ## Run the linter on all non-generated source files
	$(LINTER) $(LINTER_FLAGS) $^
.PHONY: lint

doc: dist/doc/html/emperor/emperor/index.html ## Make the documentation
.PHONY: doc

open-doc: dist/doc/html/emperor/emperor/index.html ## Open the documentationin the default browser
	$(OPEN) $<
.PHONY: open-doc

dist/doc/html/emperor/emperor/index.html: $(SOURCE_FILES)
	cabal haddock --executables

clean-installation: ## Remove installed executables, libraries and documentation
	sudo $(RM) /usr/bin/emperor
	sudo $(RM) /usr/share/man/man1/emperor.1.gz
	sudo $(RM) /usr/share/bash-completion/completions/emperor 2>/dev/null || true
	$(RM) -r $(shell emperor-setup -C)
	$(RM) -r $(shell emperor-setup --language-header-location)
.PHONY: clean-installation

clean: ## Delete all generated files
	cabal clean --verbose=0
	$(RM) cabal.config Args.hs *_completions.sh ./emperor ./Parser/Emperor{Lexer,Parser,ParserData}.hs ./Parser/EmperorParser.info $(shell find . -name '*.orig') $(shell find . -name '*.info') $(shell find . -name '*.hi') *.eh*
.PHONY: clean

# Thanks, François Zaninotto! https://marmelab.com/blog/2016/02/29/auto-documented-makefile.html
help: ## Output this help summary
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
.PHONY: help
