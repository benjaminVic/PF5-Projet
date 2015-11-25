open String
open Types

(* Ouverture du fichier *)
let fichier = "automate.txt";;
let in_chanel = open_in fichier;;

(* Affiche une liste *)
let rec print_list = function
	|[] -> print_newline()
	| a::t -> print_string a; print_string " "; print_list t
;;

(* Récupère les rule depuis le fichier text en retourne un automaton*)
let getRules in_chanel =
  let rec getRulesAux in_chanel automatonResult=
    let line = input_line in_chanel in
    if (String.compare line "GenerationZero") != 0 then
      begin
	(*print_endline line;*)
	let rule = stringToRule line in
	getRulesAux in_chanel automatonResult@[rule];
      end
    else(automatonResult)
  in getRulesAux in_chanel []
;;

(* Récupère la première ligne du fichier *)
let getSizeGrid in_chanel = 
	let line = input_line in_chanel in
		int_of_string line
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
			if (String.compare	 line "Regles") = 0 then
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

let sizeGrid = 7;;

(* Modifie une cellule x;y dans une génération g avec l'état s *)
let setState g x y s = 
	g.(x).(y) <- s;
;;

(* Affiche un etat s *)
let print_state s = match s with
| A -> print_string " A "
| D -> print_string " D "
;;

(* Affiche une génération g *)
let show_generation g =
	print_endline "---------------------";
	for i = 0 to sizeGrid-1 do
	begin
		for j = 0 to sizeGrid-1 do
			print_state g.(j).(i)
		done;
		print_newline();
	end
	done;
	print_endline "---------------------";
;;

(* Retourne le prochain état d'une cellule en fonction de son voisinage *)
let rec next_state listeRules (n,e,s,o,cell) = match listeRules with
	|[] -> cell
	|a::t -> match a with 
		|(a,b,c,d,f) when (a=n && b=e && c=s && d=o && f=cell) -> if cell = A then D else A
		| _ -> next_state t (n,e,s,o,cell) 
;;

(* Retourne la prochaine génération g calculée à partir d'un automate et d'une génération *)
let next_generation a (g : state array array) = 
	let generationTemp = Array.make_matrix 7 7 A in 
		for i=0 to sizeGrid-1 do
		begin
			for j=0 to sizeGrid-1 do
			begin
				let voisins = 
					((if i-1 < 0 then g.(sizeGrid-1).(j) else g.(i-1).(j)),
					(if j+1 > sizeGrid-1 then g.(i).(0) else g.(i).(j+1)),
					(if i+1 > sizeGrid-1 then g.(0).(j) else g.(i+1).(j)),
					(if j-1 < 0 then g.(i).(sizeGrid-1) else g.(i).(j-1)),
					g.(i).(j))
					in
						generationTemp.(i).(j) <- next_state a voisins 
			end
			done
		end
		done;
		generationTemp
;;

let rule1 = (A,A,A,A,A);;
let rule2 = (A,A,A,A,D);;
let rule3 = (A,A,A,D,A);;
let rule4 = (A,A,A,D,D);;

let automaton = rule1::rule2::rule3::rule4::[];;

let generationZero = Array.make_matrix 7 7 A;; 
setState generationZero 0 0 D;;
setState generationZero 1 1 D;;
setState generationZero 2 2 D;;
setState generationZero 3 3 D;;
setState generationZero 4 4 D;;
setState generationZero 5 5 D;;
setState generationZero 6 6 D;;

let generationUne = next_generation automaton generationZero;;

show_generation generationZero;;
show_generation generationUne;;