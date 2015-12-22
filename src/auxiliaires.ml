open String
open Types
open Affichage

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
;;
