(*Naloga 2.26*)

let sez1 = [1;2;3]
let sez2 = [5;6;7;8]

let rec zdruzi (sez1,sez2) = match (sez1,sez2) with
| ([],x) -> x (*preveri ali je prvi seznam prazen, lahko sta tudi oba*)
| (x,[]) -> x (*preveri ali je drugi seznam prazen*)
| (g1::[],g2::r2) -> g1::g2::r2 (*seznam 1 ima samo še en element*)
| (g1::r1,g2::[]) -> g1::g2::r1 (*seznam 2 ima samo še en element*)
| (g1::r1,g2::g22::r2) -> g1::g2::g22:: zdruzi (r1,r2);; (* vsi ostali primeri *)

zdruzi (sez1,sez2)
zdruzi ([1;2;3],[5;6;7;8;9;0;4;8])
(*Naloga 3.27*)
let rec cnta sez = match sez with
| [] -> []
| 'a'::'a'::'a'::r -> 3 :: cnta r (* aaa *)
| 'a'::'a'::r -> 2 :: cnta r (* aa *)
| 'a'::r  -> 1 :: cnta r (* a *)
| _::r -> 0 :: cnta r;; (* ostalo *)

cnta  ['a';'a';'a';'t';'a';'b';'a';'a'];;
cnta ['b'
(*Naloga .5*)
type ('a,'b) drevo =
	Prazno
	|	Vozliscea of 'a * ('a,'b) drevo
	| Vozlisceb of 'b * ('a,'b) drevo;;
let dr1 = Vozliscea (9, Vozlisceb ("je", Vozlisceb ("bo", Prazno)));;
(*Rezultat ([9],["je";"bo"]*)
let razcepi drevo =
	let a = ref [] in 
	let b = ref [] in
	let rec raz dr = match dr with
	| Prazno -> (!a,!b)
	| Vozliscea (x,y) -> a := !a @ [x]; raz y
	| Vozlisceb (x,y) -> b := !b @ [x]; raz y
	in
	raz drevo;;

razcepi dr1;;
(*Naloga 4.7*)
(* tip *)
type izraz =
	| Nil
	| Stevilo  of  int * izraz
	| Oper  of  char * izraz;;
(* primer *)
let e = Stevilo(10, Oper('+', Stevilo(5, Oper('-', Stevilo(3, Nil )))));;

let ovrednoti izraz =
let x = ref 0 in
	let rec ovred izr = match izr with
	| Nil -> !x (*vrne izračunano vrednost*)
	| Stevilo (a,iz) when (!x = 0)-> x := a ; ovred iz (*Prvi elemnt izraza*)
	| Oper (t, Stevilo (a,iz)) when (t = '-') -> x:= !x - a; ovred iz (*operator -*)
	| Oper (t, Stevilo (a,iz)) when (t = '+') -> x:= !x + a; ovred iz (*operator +*)
	in
	ovred izraz;;

ovrednoti (Stevilo(5, Oper('-', Stevilo(5, (Oper('-', Stevilo(5, (Nil) )) )))));;
let naredi x = 
	let i = 2 in 
	let nar = (fun t -> t+ 3) 
	in 
	nar (x +i);;

naredi 6;;


