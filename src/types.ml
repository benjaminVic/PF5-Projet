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
type neighbors = state * state * state * state * state;;
(* =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= *)

