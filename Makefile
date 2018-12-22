CC  = ocamlc
BASE= str.cma graphics.cma
OBJ = code.cmo alg_knuth.cmo imple_graphique.cmo graphic.cmo
EXE = game

all:
	@echo [BUILDING...]
	cd src/ && ocamlc str.cma graphics.cma -c code.ml
	cd src/ && ocamlc str.cma graphics.cma code.cmo -c alg_knuth.ml
	cd src/ && ocamlc str.cma graphics.cma code.cmo alg_knuth.cmo -c graphic.ml
	cd src/ && $(CC) $(BASE) -c imple_graphique.ml
	cd src/ && $(CC) $(BASE) $(OBJ) -o $(EXE)
	@echo [BUILDING FINISHED]
