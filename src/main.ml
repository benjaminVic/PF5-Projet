let fichier = "automate.txt";;
let in_chanel = open_in fichier;;

let rec print_file () = 
	try
		let line = input_line in_chanel in
			print_endline line;
			flush stdout;
			print_file ();

	with 
	| End_of_file -> print_string("Fin du fichier.\n");
	| e -> close_in_noerr in_chanel;
				 raise e;
;;	


print_file ();