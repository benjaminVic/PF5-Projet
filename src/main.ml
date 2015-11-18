open String

(* Définition des types *)
type state = A | D ;;
type generation = state array array;;
type rule = string ;;
type automaton = rule list;;

(* Ouverture du fichier *)
let fichier = "automate.txt";;
let in_chanel = open_in fichier;;

(* Informations du fichier *)
let sizeGrid = ref 0;;

(* Fonctions de parsing *)
let parse in_chanel = 
	try
		(* Récupération de la taille *)
		let line = input_line in_chanel in
			sizeGrid := int_of_string line;

		(* Récupération des règles *)
		let line = input_line in_chanel in
			if (String.compare line "Regles") = 0 then 
				begin 
					let line = input_line in_chanel in
						print_endline line;
			end else print_endline "Fichier malformé.";

	with 
	| End_of_file -> print_string("Fin du fichier.\n");
	| e -> close_in_noerr in_chanel; raise e;
;;


parse in_chanel;;

print_int !sizeGrid;;
print_newline();;

