open String
open Types
open Affichage
open Formules

(* Transforme un char en state*)
let charToState s = match s with 
  | 'A' -> A
  | 'D' -> D
  | _ -> failwith("This shouldn't be read")
;;

(* Transforme une string en rule *)
let rec stringToRule s =
  if (length s = 5) then
    charToState( get s 0),charToState( get s 1),charToState( get s 2),charToState( get s 3),charToState( get s 4)
  else raise File_structure;
;;

(* Récupère la première ligne du fichier *)
let getSizeGrid in_chanel = 
  let line = input_line in_chanel in
  int_of_string line
;;

(*Avance le flux de lecture jusqu'aux regles *)
let rec advanceToRules in_chanel = 
  try
    let line = input_line in_chanel in
    if (String.compare line "Regles") = 0 then print_string ("Mise à niveau du flux de lecture aux rêgles.\n")
    else advanceToRules in_chanel;
  with 
  | End_of_file -> print_string ("Fichier malformé au niveau des rêgles");
;;

(* Récupère les rule depuis le fichier text en retourne un automaton*)
let getRules in_chanel =
  (* On se place au début des valeurs *)
  advanceToRules in_chanel;
  let rec getRulesAux in_chanel automatonResult=
    let line = input_line in_chanel in
    (*On s'arrete à la générationZero*)
    if (String.compare line "GenerationZero") != 0 then
      begin
	(*print_endline line;*)
	let rule = stringToRule line in
	getRulesAux in_chanel automatonResult@[rule];
      end
    else(automatonResult)
  in getRulesAux in_chanel []
;;

(* Modifie une cellule x;y dans une génération g avec l'état s *)
let setState g x y s = 
  g.(x).(y) <- s;
;;

(* Récupère la génération Zero *)
let getGenerationZero generationTemp in_chanel sizeGrid = 
  (* Affecte les valeurs au futur automaton *)
  let rec getGenerationZeroAuxColums generationTemp in_chanel sizeGrid lNumber =
    if lNumber<sizeGrid then
      begin
      	try
      	  let line = input_line in_chanel in
      	  (*Parcours la ligne et affecte les valeurs*)
      	  let rec getGenerationZeroAuxLine generationTemp in_chanel sizeGrid lNumber cNumber =
      	    if ((length line) = sizeGrid) then
      	      if (cNumber < sizeGrid) then
      		begin
      		  setState generationTemp lNumber cNumber (charToState(get line cNumber));
      		  getGenerationZeroAuxLine generationTemp in_chanel sizeGrid lNumber (cNumber+1);
      		end
      	      else (getGenerationZeroAuxColums generationTemp in_chanel sizeGrid (lNumber+1))
      	    else raise File_structure;
      	  in (getGenerationZeroAuxLine generationTemp in_chanel sizeGrid lNumber 0);
      	with 
      	| End_of_file -> print_string("Fin du fichier.\n"); ();
      	| File_structure -> print_string("Fichier malformé.\n");
      	| e -> close_in_noerr in_chanel; raise e;
      end
    else print_string("Fin de la récupération de la générationZero\n");
  in getGenerationZeroAuxColums generationTemp in_chanel sizeGrid 0;
  generationTemp
;;

(* Traduit un état en une valeur logique *)
let stateToFormule s direction= match s with
  | A -> Var direction
  | D -> Neg(Var direction)
(* | _ -> failwith("This shouldn't happen") *)
;;

(* Traduit une rêgle en formule *)
let ruleToFormule r cellIdNumber gridSize = match r with
  | (n,e,s,w,c) -> Et(Et(Et(Et(
    stateToFormule n (string_of_int (if (cellIdNumber-gridSize)<0 then (((gridSize*gridSize)-gridSize)+cellIdNumber)+1  else (cellIdNumber-gridSize)+1)),
    stateToFormule e (string_of_int (if (cellIdNumber+1) mod gridSize = 0 then (cellIdNumber-gridSize+1)+1 else (cellIdNumber+1)+1) )),
    stateToFormule s (string_of_int (if (cellIdNumber+gridSize)>=(gridSize*gridSize) then (cellIdNumber mod gridSize)+1 else (cellIdNumber+gridSize)+1))),
    stateToFormule w (string_of_int (if ((cellIdNumber-1) mod gridSize = (gridSize-1) || (cellIdNumber-1)<0 ) then (cellIdNumber+gridSize-1)+1 else (cellIdNumber-1)+1) )), 
    stateToFormule c (string_of_int (cellIdNumber+1)))
;;

(* Converti un automaton en formule *)
let rec automatonToFormule automaton cellIdNumber gridSize = match automaton with
  | [] -> VRAI
  | [b] -> ruleToFormule b cellIdNumber gridSize;
  | h::t -> Ou(ruleToFormule h cellIdNumber gridSize, automatonToFormule t cellIdNumber gridSize);
;;

let generationToVars gridSize automaton = 
  let rec generationToVarsAux gridSize cellIdNumber automaton =
    if cellIdNumber < ((gridSize*gridSize)-1) then 
      Et((generationToVarsAux gridSize (cellIdNumber+1) automaton), 
        Ou(
              Et(Var (string_of_int (cellIdNumber+1)), automatonToFormule automaton cellIdNumber gridSize), 
          Neg(Et(Var (string_of_int (cellIdNumber+1)), automatonToFormule automaton cellIdNumber gridSize))
          )
      )
    else
      Ou(
        Et(Var (string_of_int (cellIdNumber+1)), automatonToFormule automaton cellIdNumber gridSize), 
        Neg(Et(Var (string_of_int (cellIdNumber+1)), automatonToFormule automaton cellIdNumber gridSize))
      )
  in generationToVarsAux gridSize 0 automaton
;;

(* generationToVars 5 [(A,A,A,A,A);(A,A,A,A,D)];;*)

(* Liste to string *)
let rec string_of_StringList = function
  |[] -> ""
  |[a] -> a
  |a::t -> a^" "^(string_of_StringList t)
;;

(* CNF to ListeFormule *)
let rec cnf_to_disjonctionListe f = match f with
  |VRAI -> [VRAI]
  |FAUX -> [FAUX]
  |Var(_) as v -> [v]
  |Neg(_) as g -> [g]
  |Ou(_,_) as f -> [f]
  |Et(a,b) -> b::(cnf_to_disjonctionListe a)
;;

(* Permet l'inversion de tout les signes *)
let rec negative_string_liste = function
  |[] -> []
  |[a] when String.compare a "0" = 0 -> [a]
  |[a] when String.compare a "0" != 0 -> if String.compare (String.sub a 0 1) "-" = 0 then [String.sub a 1 ((String.length a)-1)] else ["-"^a]
  |a::t -> (negative_string_liste [a])@(negative_string_liste t)
;;

(* Ecrit la liste des solutions trouées *)
let rec write_solutions_to_files out_channel = function
  |[] -> ()
  |[a] -> output_string out_channel (a^"\n")
  |a::t -> output_string out_channel (a^"\n"); write_solutions_to_files out_channel t
;;



