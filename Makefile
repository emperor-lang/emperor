#!/usr/bin/make
SHELL := /bin/bash

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
FORMATTER := stylish-haskell
FORMATTER_FLAGS := -i

HEADER_INSTALL_DIRECTORY = $(shell emperor-setup --language-header-location)
COMPLETION_INSTALL_LOCATION = /usr/share/bash-completion/completions/emperor

.DEFAULT_GOAL := all

# All required source files (existent or otherwise)
SOURCE_FILES = $(shell find . -name '*.hs' | grep -v dist) ./Args.hs ./Parser/EmperorLexer.hs ./Parser/EmperorParser.hs

all: build ## Build everything
.PHONY: all

build: ./emperor ## Build everything, explicitly
.PHONY: build

./emperor: ./.stack-work/install/x86_64-linux-tinfo6/a4fefd2a9618441c5b464352bd9d27949d738f84f553d0be92299367e59678e1/8.6.5/bin/emperor
	@echo "[[ ! -f $@ ]] && ln -sf $^ $@"
	$(shell [[ ! -f $@ ]] && ln -sf $^ $@)
.DELETE_ON_ERROR: ./emperor

./.stack-work/install/x86_64-linux-tinfo6/a4fefd2a9618441c5b464352bd9d27949d738f84f553d0be92299367e59678e1/8.6.5/bin/emperor: $(SOURCE_FILES)
	stack build

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

install: /usr/bin/emperor /usr/share/man/man1/emperor.1.gz $(COMPLETION_INSTALL_LOCATION) # $(DEFAULT_HEADERS) ## Install binaries, libraries and documentation
.PHONY: install

/usr/bin/emperor: ./.stack-work/install/x86_64-linux-tinfo6/a4fefd2a9618441c5b464352bd9d27949d738f84f553d0be92299367e59678e1/8.6.5/bin/emperor
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
