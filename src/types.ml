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
type neighbors = state * state * state * state * state;;
(* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= *)

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
  else failwith("This line should not be read")
;;

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
