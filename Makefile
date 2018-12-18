CC=ocamlc
BASE=str.cma
OBJ = load.cmo code.cmo alg_knuth.cmo alg_naif.cmo ia.cmo main.cmo
EXE = game

all:
	@echo [BUILDING...]
	cd src/ && $(CC) $(BASE) -c load.ml
	cd src/ && $(CC) $(BASE) -c alg_naif.ml
	cd src/ && $(CC) $(BASE) -c alg_knuth.ml
	cd src/ && $(CC) $(BASE) -c code.ml
	cd src/ && $(CC) $(BASE) -c main.ml
	cd src/ && $(CC) $(BASE) -c ia.ml
	cd src/ && $(CC) $(BASE) $(OBJ) -o $(EXE)
	@echo [BUILDING FINISHED]
