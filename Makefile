CC=ocamlc
BASE=str.cma

all:
	@echo [BUILDING...]
	cd src/ && $(CC) $(BASE) load.ml code.ml main.ml -o ../bin/game
	@echo [BUILDING FINISHED]
