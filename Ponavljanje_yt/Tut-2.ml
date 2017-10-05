(*lists*)
(*v listu vedno isti tipi*)
(**)

let x = ['a';'b'];;

let y = 'c'::x;; (*dodamo na spredni del oz prvo mesto lista
ne moremo dodajat na koncu lista*)
(*liste ne moramo spreminjati drugace.*)

let z = x @ y;;
let z = y @ x;;

List.hd z;;
List.tl z;;

(**)
(* *)
(* funkcije nad listi*)
(* *)
(* *)

(*iskanje elementa v seznamu*)
let rec has_element list element = match list with
| [] -> false
| h::t -> if element=h then true else has_element t element;;

has_element z 's';;
has_element [1;2;3;4;5;6] 2;;

let has = has_element z 's';;
(*dupliciranje lista, oziroma podvoji elemente v listu*)
let rec duplicate_list list= match list with
| [] -> []
| h::t -> h::h::duplicate_list t;;

duplicate_list z;;
duplicate_list [1;2;3;4;5;6];;

(*obracanje lista*)
let rec reverse_list list revlist = match list with
| [] -> revlist
| h::t -> reverse_list t revlist@[h];;

reverse_list [1;2;3;4;5;6] [];;

(*obracanje lista k sm ga js naredu :) *)
let rec rev list = match list with
| [] -> []
| h::t -> rev t@[h];;

rev [1;2;3;4;5;6];;


(**)
(* *)
(* vec funkcij*)
(* *)
(* *)
(*find an element at given index*)
let rec element_at mylist index = match mylist with
| [] -> raise(Failure "empty list")
| h::t -> if index = 0 then h else element_at t (index-1);;

element_at [1;2;3;4;5;6;7] 10;;

(*uporaba pogojev z pattern matching*)
let rec element_at mylist index = match mylist with
| [] -> raise (Failure "empty list or out of bounds")
| h::_ when index = 0 -> h
| _::t -> element_at t (index-1);;

element_at [1;2;3;4;5;6;7] 2;;

(*vsi inti med dvema steviloma*)
let rec range a b result= 
	if a = b then result@[b]
	else if a > b then []
	else range (a+1) b (result@[a]);;

range 2 5 [];;


let range a b = 
	let rec range2 a b =
		if a > b then []
		else a::range2 (a+1) b
	in 
		range2 a b;;

range 2 5;;

(*list moduli*)
let l = [1;2;3;4;5;6;7;8;9];;

List.length l;;
List.nth l 4;;
List.rev l;;

let ll = [10;11;12;13;14;15];;
List.append l ll;;
List.rev_append l ll;;

let ls = [l;ll;[16;17;18]];;
List.concat ls;; (*splosci jih*)

