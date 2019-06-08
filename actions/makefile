#!/usr/bin/make
CC = gcc
OUTPUT_FILE = actions.o

.PHONY: all;

all: actions.o;

actions.o:
	$(CC) -c actions.c -o $(OUTPUT_FILE)

clean:
	-@ rm $(OUTPUT_FILE)		2>/dev/null	|| true
