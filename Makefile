CC=ocamlc
BASE=str.cma

libs:
	@echo [BUILDING MODULES]
	$(CC) $(BASE) src/code.ml -o bin/obj/code.cmo

all: libs
	$(CC) $(BASE) bin/obj/* src/test.ml -o bin/final
