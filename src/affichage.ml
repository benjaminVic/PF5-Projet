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
let show_generation g sizeGrid =
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

(* Affiche une rule *)
let show_rule (n,e,s,w,c) =
print_state n; print_state e; print_state s; print_state w ;print_state c
;;

(* Affiche un automaton*)
let show_automaton a =
  print_endline "---------------------";  
  print_endline "rêgles"; 
  let lenght = List.length a in
  for i = 0 to lenght-1 do   
    begin 
      let r = List.nth a i in     
      show_rule r;
      print_newline();
    end
  done;
  print_endline "---------------------";
;;
