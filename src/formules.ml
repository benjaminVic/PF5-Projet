type formule = VRAI | FAUX
	       | Var of string
	       | Neg of formule
	       | Et of formule * formule
	       | Ou of formule * formule;;

let rec string_of_formule f = match f with
  | VRAI -> "Vrai"
  | FAUX -> "Faux"
  | Var v -> v
  | Neg g -> "Neg("^string_of_formule g^")"
  | Et (f1,f2) -> "("^string_of_formule f1^" ET "^string_of_formule f2^")"
  | Ou (f1,f2) -> "("^string_of_formule f1^" OU "^string_of_formule f2^")"
;;

let get_number_of_var f = 
  let number = ref 0 in
  let rec count f = match f with
    | VRAI -> number := !number + 1
    | FAUX -> number := !number + 1
    | Var v -> number := !number + 1
    | Neg g -> (count g)
    | Et (f1,f2) -> (count f1); (count f2)
    | Ou (f1,f2) -> (count f1); (count f2)
  in count f;
  !number
;;

(*Might have to replace l3 by l1 and l4 by l2*)
let rec union_sorted l1 l2 = match l1,l2 with
  | _,[] -> l1
  | [],_ -> l2
  | a1::l3,a2::l4 ->
    if a1 < a2 then a1::(union_sorted l3 l2 ) else
      if a2 < a1 then a2::(union_sorted l1 l4) else
      a1::(union_sorted l3 l4)
;;

let rec list_of_vars f = match f with
  | Var s -> [s]
  | Neg f -> list_of_vars f
  | Et(f1,f2) | Ou(f1,f2) -> union_sorted (list_of_vars f1) (list_of_vars f2)
  | _ -> []
;;

let rec eval_formule f e = match f with
  | VRAI -> true
  | FAUX -> false
  | Var v(*une string*) -> List.assoc v e
  | Neg f1 -> not(eval_formule f1 e)
  | Et(f1,f2) -> (eval_formule f1 e) && (eval_formule f2 e)
  | Ou(f1,f2) -> (eval_formule f1 e) || (eval_formule f2 e)
;;

let rec eval_sous_formule f = match f with
  | VRAI -> VRAI
  | FAUX -> FAUX
  | Var v -> f
  | Neg(g) -> if g = VRAI then FAUX else VRAI
  | Et(f1,f2) -> if(eval_sous_formule f1 = VRAI && eval_sous_formule f2 = VRAI)
    then VRAI else FAUX
  | Ou(f1,f2) -> if (eval_sous_formule f1 = VRAI || eval_sous_formule f2 = VRAI)
    then VRAI else FAUX
;;

let simplifie_formule f = match f with
  | Et(f1,f2) -> begin
    match (eval_sous_formule f1, eval_sous_formule f2) with
    | FAUX,_ -> FAUX
    | _,FAUX -> FAUX
    | VRAI,VRAI -> VRAI
    | (f1,f2) -> Et(f1,f2)
  end
  | Ou(f1,f2) -> begin
    match (eval_sous_formule f1, eval_sous_formule f2) with
    | VRAI,_ -> VRAI
    | _,VRAI -> VRAI
    | FAUX,FAUX -> FAUX
    | (f1,f2) -> Ou(f1,f2)
  end
  | Neg(f) -> begin
    match eval_sous_formule f with
    | VRAI -> FAUX | FAUX -> VRAI
    | f -> Neg f
  end
  | f -> f
;;

let rec desc_n f = match f with
  | Neg(Neg(g)) -> desc_n g
  | Neg(Ou(f1,f2)) -> Et(desc_n(Neg(f1)),desc_n(Neg(f2)))
  | Neg(Et(f1,f2)) -> Ou(desc_n(Neg(f1)),desc_n(Neg(f2)))
  | Et(f1,f2) -> Et(desc_n f1, desc_n f2) 
  | Ou(f1,f2) -> Ou(desc_n f1, desc_n f2)
  | f -> f
;;

let rec desc_ou f = match f with
  | Et(g,h) -> Et(desc_ou g, desc_ou h)
  | Ou(g,h) ->
    let g1 = desc_ou g
    and h1 = desc_ou h in
    (match g1,h1 with
      _,Et(g,h) -> Et(desc_ou (Ou(g1,g)),desc_ou (Ou(g1,h)))
    | Et(f,g),_ -> Et(desc_ou (Ou(f,h1)),desc_ou (Ou(g,h1)))
    | _ -> Ou(g1,h1))
  | f -> f
;;

let fnc f = desc_ou(desc_n f);;


