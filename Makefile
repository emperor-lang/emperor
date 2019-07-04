#!/usr/bin/make

CC = gcc-8
CFLAGS := $(shell emperor-setup --cflags) # $(CFLAGS) -Wall -Os -I . -I /usr/include/python3.6m -g
CLIBS := $(shell emperor-setup --libs)

HC := ghc
HC_FLAGS := -Wall -Werror -O2

.DEFAULT_GOAL := all

all: emperor ;
.PHONY: all 

emperor: ./emperor.hs ./Args.hs
	$(HC) $(HC_FLAGS) $^ -o $@
.DELETE_ON_ERROR: emperor

emperor.hs:;

./Args.hs: ./emperor.json
	arggen_haskell < $^ > $@

install: /usr/bin/emperor /usr/share/man/man1/emperor.1.gz;
.PHONY: install

/usr/bin/emperor: emperor
	sudo install -m 755 $^ $@

/usr/share/man/man1/emperor.1.gz: emperor.1.gz
	sudo install -m 644 $^ $@

man: emperor.1.gz;
.PHONY: man

emperor.1.gz: emperor.json
	(mangen | gzip --best) < $^ > $@
.DELETE_ON_ERROR: emperor.1.gz

clean-installation:
	sudo $(RM) /usr/bin/emperor
	sudo $(RM) /usr/share/man/man1/emperor.1.gz
.PHONY: clean-installation

clean:
	-@$(RM) emperor			2>/dev/null	|| true
	-@$(RM) emperor.1.gz	2>/dev/null	|| true
	-@$(RM) *.hi			2>/dev/null	|| true
	-@$(RM) *.o				2>/dev/null	|| true
	-@$(RM) Args.hs			2>/dev/null	|| true
.PHONY: clean
