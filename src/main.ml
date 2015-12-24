open String
open List

open Types
open Affichage
open Auxiliaires

(* Construit l'automate contenu dans le fichier *)
let parse in_chanel = 
  let gridSize = getSizeGrid in_chanel in
  let rule = getRules in_chanel in
  let generationTemp = Array.make_matrix gridSize gridSize D in
  let genZero = getGenerationZero generationTemp in_chanel gridSize in
  (gridSize),(rule),(genZero);
;;

(* Sauvegarde les éléments du parsing dans des références *)
let saveToRef refS refA refG = function
	|(s,a,g) -> (refS := s); (refA := a); (refG := g)
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

(* ############################################## *)
(* AVEC PARSING *)
(* ############################################## *)

(* Ouverture du fichier *)
let fichier = "automate.txt";;
let in_chanel = open_in fichier;;

(* Initialisation des références *)
let sizeGrid = ref 0;;
let automaton = ref [];;
let generationZero = ref ();;

(* Parsing du fichier *)
saveToRef sizeGrid automaton generationZero (parse in_chanel);;

print_int !sizeGrid;;
print_newline();;


(* ############################################## *)
(* SANS PARSING DE FICHIER *)
(* ############################################## *)

(*
let sizeGrid = 7;;

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

let generationUne = next_generation sizeGrid automaton generationZero;;

show_generation generationZero sizeGrid;;
show_generation generationUne sizeGrid;;

*)