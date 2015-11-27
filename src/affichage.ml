open Types

(* Affiche une liste *)
let rec print_list = function
	|[] -> print_newline()
	| a::t -> print_string a; print_string " "; print_list t
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
