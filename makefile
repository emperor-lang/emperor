CC = gcc-8
CFLAGS := $(CFLAGS) -Wall -Os -I . -I /usr/include/python3.6m -g
CYTHON := cython3
CYTHON_FLAGS := --embed --directive language_level=3
CP = cp
OUTPUT_FILE := ./emperor
MANGEN_LOCATION := ../mangen/
MANGEN := $(MANGEN_LOCATION)mangen
# MAKEFLAGS := $(MAKEFLAGS) s

MAN_FILE := ./emperor.1.gz

EXECUTABLE_INSTALL_LOCATION := /usr/bin/emperor
MAN_INSTALL_LOCATION := /usr/share/man/man1/emperor.1.gz

.PHONY: all clean actions parser run man install clean-installation

all: $(OUTPUT_FILE) ;

$(OUTPUT_FILE): ./emperor.py.c # ./parser/parser.so ./actions/actions.o
	$(CC) $(CFLAGS) ./emperor.py.c -o $(OUTPUT_FILE) -lpython3.6m # ./parser/parser.so ./actions/actions.o -lpython3.6m -o $(OUTPUT_FILE)

./%.o: ./%.c ./%.h

./actions/%.o:
	@$(MAKE) actions

./parser/parser.so:
	@$(MAKE) parser

./emperor.py.c: ./emperor.pyx 
	$(CYTHON) $(CYTHON_FLAGS) ./emperor.pyx -o ./emperor.py.c

actions:
	@$(MAKE) -C actions

parser:
	@$(MAKE) -C parser

./emperor.pyx:

install: $(EXECUTABLE_INSTALL_LOCATION) $(MAN_INSTALL_LOCATION);

$(EXECUTABLE_INSTALL_LOCATION): $(OUTPUT_FILE)
	sudo install $(OUTPUT_FILE) $(EXECUTABLE_INSTALL_LOCATION)

$(MAN_INSTALL_LOCATION): $(MAN_FILE)
	sudo install -m 644 $(MAN_FILE) $(MAN_INSTALL_LOCATION)

man: $(MAN_FILE)

$(MAN_FILE): $(OUTPUT_FILE) $(MANGEN)
	./emperor -* | $(MANGEN) - | gzip --best > $(MAN_FILE)

$(MANGEN):
	+@$(MAKE) -C $(MANGEN_LOCATION) install

clean-installation: clean
	-@ sudo $(RM) $(MAN_INSTALL_LOCATION)
	-@ sudo $(RM) $(EXECUTABLE_INSTALL_LOCATION)

clean:
	+@$(MAKE) -C actions $@
	+@$(MAKE) -C parser $@
	-@$(RM) $(MAN_FILE)						2>/dev/null	|| true
	-@$(RM) *.o								2>/dev/null	|| true
	-@$(RM) *.so							2>/dev/null	|| true
	-@$(RM) emperor.c						2>/dev/null	|| true
	-@$(RM) Python.h						2>/dev/null	|| true
	-@$(RM) $(OUTPUT_FILE)					2>/dev/null	|| true
	-@$(RM) -R build						2>/dev/null	|| true
