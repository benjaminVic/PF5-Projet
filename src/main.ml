open String
open List

open Types
open Affichage
open Auxiliaires
open Formules

(* Construit l'automate contenu dans le fichier *)
let parse in_chanel = 
  let gridSize = getSizeGrid in_chanel in
  let rule = getRules in_chanel in
  let generationTemp = Array.make_matrix gridSize gridSize D in
  let genZero = getGenerationZero generationTemp in_chanel gridSize in
  (gridSize),(rule),(genZero);
;;

(* Retourne le prochain état d'une cellule en fonction de son voisinage *)
let rec next_state listeRules (n,e,s,o,cell) = match listeRules with
	|[] -> if cell = A then D else cell
	|a::t -> match a with 
		|(a,b,c,d,f) when (a=n && b=e && c=s && d=o && f=cell) -> A
		| _ -> next_state t (n,e,s,o,cell) 
;;

(* Retourne la prochaine génération g calculée à partir d'un automate et d'une génération *)
let next_generation sizeGrid a (g : state array array) = 
	let generationTemp = Array.make_matrix sizeGrid sizeGrid A in 
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

(* PRODUCE THE STABLES FORMULA *)
let stables gridSize automaton =
  Et(generationToVars gridSize, automatonToFormule automaton)
;;

let create_dimacs formula_liste out_channel = 
	output_string out_channel ("p cnf "^(string_of_int (get_nb_var_in_FormulaList formula_liste))^" "^(string_of_int (length formula_liste)));
	output_string out_channel "\n";
	let rec print_disjonction = function
		|[] -> ()
		|[a] -> output_string out_channel (string_of_StringList (list_of_vars a))
		|a::t -> (output_string out_channel ((string_of_StringList (list_of_vars a))^"\n")); print_disjonction t;
	in print_disjonction formula_liste
;;

(* Affiche résultat minisat *)
let show_stable () = 
	()
;;

(* ############################################## *)
(* PARSING et CREATION *)
(* ############################################## *)

(* Ouverture du fichier *)
let fichier_automate = "automate.txt";;
let in_channel = open_in fichier_automate;;

(* Récupération des variables *)
let sizeGrid, automaton, generationZero = (parse in_channel);;

(* Calcul de la prochaine génération *)
let generationUne = next_generation sizeGrid automaton generationZero;;

(* Affichage des générations 0 et 1 *)
show_generation generationZero sizeGrid;;
show_generation generationUne sizeGrid;;

(* ############################################## *)
(* FORMULES *)
(* ############################################## *)

(* Ouverture du fichier *)
let fichier_dimacs = "entree.dimacs";;
let out_channel = open_out fichier_dimacs;;

(* Récupération de la formule stable *)
let f = stables sizeGrid automaton;;
print_string (string_of_formule f);;

(* Transformation en CNF et écriture du fichier dimacs *)
let liste_Formules = [Ou(Ou(Var("x1"),Var("x2")),Var("x3"));Ou(Var("x4"),Var("x5"))];;

create_dimacs liste_Formules out_channel;;
