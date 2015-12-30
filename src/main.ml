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

let stables gridSize automaton = 
  fnc(generationToVars gridSize automaton)
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

let entree_dimacs = "entree.dimacs";;
let sortie_dimacs = "sortie";;
let out_channel = open_out entree_dimacs;;
let in_channel = open_in sortie_dimacs;;

(* Récupération de la formule stable *)
let f = stables sizeGrid automaton;;
let fTest = Et(Et(Et( Ou(Ou(Var("2"),Var("3")), Neg(Var("1"))) ,Ou(Var("1"),Neg(Var("3")))), Ou(Var("1"),Var("2")) ), Var("3"));;

(* Mise sous forme de liste de disjonctions *)
let liste_Formules = cnf_to_disjonctionListe fTest;;

let create_dimacs formula_liste out_channel = 
	output_string out_channel ("p cnf "^(string_of_int (sizeGrid*sizeGrid))^" "^(string_of_int (length formula_liste)));
	output_string out_channel "\n";
	let rec print_disjonction = function
		|[] -> ()
		|[a] -> output_string out_channel ((string_of_var_NNF a)^"0")
		|a::t -> (output_string out_channel ((string_of_var_NNF a)^"0\n")); print_disjonction t;
	in print_disjonction formula_liste
;;

let rec negative_string_liste = function
	|[] -> []
	|[a] when String.compare a "0" = 0 -> [a]
	|[a] when String.compare a "0" != 0 -> if String.compare (String.sub a 0 1) "-" = 0 then [String.sub a 1 ((String.length a)-1)] else ["-"^a]
	|a::t -> (negative_string_liste [a])@(negative_string_liste t)
;;

(* Affiche résultat minisat *)
let show_stable ()= 
	create_dimacs liste_Formules out_channel;
	close_out out_channel;
	Sys.command("minisat -verb=0 entree.dimacs sortie");
	if (String.compare (input_line in_channel) "UNSAT") = 0 then print_string "Il n'y a plus de générations stables.\n"
	else 
		begin
			let line = (input_line in_channel) in 
			let splitLine = Str.split (Str.regexp " ") line in
			print_string ((string_of_StringList (negative_string_liste splitLine))^"\n");
		end
;;

show_stable ();;

(* TEST
let ftest = generationToVars 2 [A,A,A,A,A];;
let liste = cnf_to_disjonctionListe ftest;;
printListeFormule liste;;
*)