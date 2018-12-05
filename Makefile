CC=ocamlc
BASE=str.cma

all:
	@echo [BUILDING...]
	cd src/ && $(CC) $(BASE) load.ml code.ml main.ml -o ../bin/game
	cd src/ && rm *.cmo
	cd src/ && rm *.cmi
	@echo [BUILDING FINISHED]
