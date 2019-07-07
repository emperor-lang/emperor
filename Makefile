#!/usr/bin/make

# CC = gcc-8
# CFLAGS := $(shell emperor-setup --cflags) # $(CFLAGS) -Wall -Os -I . -I /usr/include/python3.6m -g
# CLIBS := $(shell emperor-setup --libs)

# HC := ghc
# HC_FLAGS := -Wall -Wextra -Werror -O2

COMPLETION_INSTALL_LOCATION := /usr/share/bash-completion/completions/emperor

.DEFAULT_GOAL := all

all: build
.PHONY: all

run: build
	@cabal run
.PHONY: run

build: ./dist/build/emperor/emperor
.PHONY: build

./dist/build/emperor/emperor: $(shell find . -name '*.hs' | grep -v dist) ./Args.hs ./parser/emperor.x.hs ./parser/emperor.y.hs ./parser/emperor.y.tab.hs
	cabal build

./parser/emperor.x.hs ./parser/emperor.y.hs ./parser/emperor.y.tab.hs: ./parser/emperor.x ./parser/emperor.y
	-@$(MAKE) -C ./parser/

./Args.hs: emperor.json
	arggen_haskell < $^ > $@

%.hs:;
%.x:;
%.y:;

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

clean-installation:
	sudo $(RM) /usr/bin/emperor
	sudo $(RM) /usr/share/man/man1/emperor.1.gz
	sudo $(RM) /usr/share/bash-completion/completions/emperor 2>/dev/null || true
.PHONY: clean-installation

clean:
	-@cabal clean			1>/dev/null || true
	-@$(RM) cabal.config	2>/dev/null || true
	-@$(RM) Args.hs			2>/dev/null	|| true
	-@$(MAKE) -sC ./parser/ clean
	-@$(RM) *_completions.sh		2>/dev/null || true
.PHONY: clean
