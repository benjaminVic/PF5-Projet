open String

(* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= *)
(* Définition des exceptions *)
exception File_structure
(* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= *)

(* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= *)
(* Définition des types *)
type state = A | D ;;
type rule = state * state * state * state * state;;
type generation = State of state array array;;
type automaton = rule list;;
(* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= *)

(* Ouverture du fichier *)
let fichier = "automate.txt";;
let in_chanel = open_in fichier;;

(* Affiche une liste *)
let rec print_list = function
	|[] -> print_newline()
	| a::t -> print_string a; print_string " "; print_list t
;;

(* Récupère la première ligne du fichier *)
let getSizeGrid in_chanel = 
	let line = input_line in_chanel in
		int_of_string line
;;

(* Récupère les règles du fichier *)
let rec getRules in_chanel =
	let line = input_line in_chanel in
		if (String.compare line "GenerationZero") != 0 then
			begin
				print_endline line;
				getRules in_chanel;
			end
		else ()
;;

(* Récupère la génération Zero *)
let rec getGenerationZero in_chanel = 
	let line = input_line in_chanel in
		print_endline line;
		getGenerationZero in_chanel;
;;

(* Construit l'automate contenu dans le fichier *)
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


(* print_list maListe; *)
(* parse in_chanel;; *)

(* ############################################## *)
(* SANS PARSING DE FICHIER *)
(* ############################################## *)

let rule1 = (A,A,A,A,A);;
let rule2 = (A,A,A,A,D);;
let rule3 = (A,A,A,D,A);;
let rule4 = (A,A,A,D,D);;

let sizeGrid = 7;;
let automaton = rule1::rule2::rule3::rule4::[];;

let generationZero = Array.make_matrix 7 7 A;; 
generationZero.(0).(0) <- D;;
generationZero.(1).(1) <- D;;
generationZero.(2).(2) <- D;;
generationZero.(3).(3) <- D;;
generationZero.(4).(4) <- D;;
generationZero.(5).(5) <- D;;
generationZero.(6).(6) <- D;;

let print_state s = match s with
| A -> print_string "A"
| D -> print_string "D"
;;

let show_generation g = 
	for i = 0 to sizeGrid-1 do
	begin
		for j = 0 to sizeGrid-1 do
			print_state g.(j).(i)
		done;
		print_newline();
	end
	done
;;

show_generation generationZero;;
