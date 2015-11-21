open String

(* Définition des exceptions *)
exception File_structure

(* Définition des types *)
type state = A | D ;;
type generation = state array array;;
type rule = string ;;
type automaton = rule list;;

(* Ouverture du fichier *)
let fichier = "automate.txt";;
let in_chanel = open_in fichier;;

(* Fonctions de parsing *)
let getSizeGrid in_chanel = 
	let line = input_line in_chanel in
		int_of_string line
;;

let rec getRules in_chanel =
	let line = input_line in_chanel in
		if (String.compare line "GenerationZero") != 0 then
			begin
				print_endline line;
				getRules in_chanel;
			end
		else ()
;;

let rec getGenerationZero in_chanel = 
	let line = input_line in_chanel in
		print_endline line;
		getGenerationZero in_chanel;
;;

let parse in_chanel = 
	try
		(* Récupération de la taille *)
		print_int (getSizeGrid in_chanel);
		print_newline();

		(* Récupération des règles *)
		let line = input_line in_chanel in
		if (String.compare line "Regles") = 0 then
			getRules in_chanel
		else raise File_structure;

		(* Récupération de la generationZero *)
		getGenerationZero in_chanel;
		
	with 
	| End_of_file -> print_string("Fin du fichier.\n");
	| File_structure -> print_string("Fichier malformé.\n");
	| e -> close_in_noerr in_chanel; raise e;
;;

parse in_chanel;;

(* Mon nouveau commentaire super utile *)