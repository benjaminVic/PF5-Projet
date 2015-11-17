let fichier = "automate.txt";;

let print_file file =
	let ic = open_in file in
	try 
		let line = input_line ic in
		print_endline line;
		flush stdout;
		close_in ic

	with e ->
		close_in_noerr ic;
		raise e
;;

print_file(fichier);