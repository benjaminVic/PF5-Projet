EXEC = AutCellulaire

all:
	ocamlc -i types.ml > types.mli
	ocamlc types.mli
	ocamlc -c types.ml
	ocamlc -i affichage.ml > affichage.mli
	ocamlc affichage.mli
	ocamlc -c affichage.ml
	ocamlc -i auxiliaires.ml > auxiliaires.mli
	ocamlc auxiliaires.mli
	ocamlc -c auxiliaires.ml
	ocamlc -c main.ml
	ocamlc -o $(EXEC) types.cmo affichage.cmo auxiliaires.cmo main.cmo

run:
	./$(EXEC)

clean:
	rm *.mli *.cmi *.cmo types.cmx