<<<<<<< HEAD
CC  = ocamlc
BASE= str.cma graphics.cma
OBJ = code.cmo alg_knuth.cmo imple_graphique.cmo graphic.cmo
=======
CC=ocamlc
BASE=str.cma
OBJ = load.cmo code.cmo alg_knuth.cmo alg_naif.cmo ia.cmo main.cmo
>>>>>>> master
EXE = game

all:
	@echo [BUILDING...]
<<<<<<< HEAD
	cd src/ && ocamlc str.cma graphics.cma -c code.ml
	cd src/ && ocamlc str.cma graphics.cma code.cmo -c alg_knuth.ml
	cd src/ && ocamlc str.cma graphics.cma code.cmo alg_knuth.cmo -c graphic.ml
	cd src/ && $(CC) $(BASE) -c imple_graphique.ml
=======
	cd src/ && $(CC) $(BASE) -c load.ml
	cd src/ && $(CC) $(BASE) -c alg_naif.ml
	cd src/ && $(CC) $(BASE) -c alg_knuth.ml
	cd src/ && $(CC) $(BASE) -c code.ml
	cd src/ && $(CC) $(BASE) -c main.ml
	cd src/ && $(CC) $(BASE) -c ia.ml
>>>>>>> master
	cd src/ && $(CC) $(BASE) $(OBJ) -o $(EXE)
	@echo [BUILDING FINISHED]
