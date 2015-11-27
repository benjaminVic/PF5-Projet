SRC_PATH = src/
BIN_PATH = bin/
EXEC = AutCellulaire

all:
	# Compilation du module types
	ocamlc -I $(SRC_PATH) -i $(SRC_PATH)types.ml > $(SRC_PATH)types.mli
	ocamlc -I $(SRC_PATH) $(SRC_PATH)types.mli
	ocamlc -I $(SRC_PATH) -c $(SRC_PATH)types.ml

	# Compilation du module affichage
	ocamlc -I $(SRC_PATH) -i $(SRC_PATH)affichage.ml > $(SRC_PATH)affichage.mli
	ocamlc -I $(SRC_PATH) $(SRC_PATH)affichage.mli
	ocamlc -I $(SRC_PATH) -c $(SRC_PATH)affichage.ml

	# Compilation du module auxiliaires
	ocamlc -I $(SRC_PATH) -i $(SRC_PATH)auxiliaires.ml > $(SRC_PATH)auxiliaires.mli
	ocamlc -I $(SRC_PATH) $(SRC_PATH)auxiliaires.mli
	ocamlc -I $(SRC_PATH) -c $(SRC_PATH)auxiliaires.ml

	# Compilation du module main
	ocamlc -I $(SRC_PATH) -c $(SRC_PATH)main.ml
	ocamlc -I $(SRC_PATH) -o $(BIN_PATH)$(EXEC) types.cmo affichage.cmo auxiliaires.cmo main.cmo

	# Clean du dossier /src
	make clean

run:
	./$(BIN_PATH)$(EXEC)

tar:
	tar zcf lefranc-vic.tar.gz bin/ src/ doc/ automate.txt Makefile Manuel.pdf README.txt

clean:
	rm $(SRC_PATH)*.mli $(SRC_PATH)*.cmi $(SRC_PATH)*.cmo