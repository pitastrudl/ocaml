    (* komentar *)
		
	let teden dan = match dan with
	| 1 -> "ponedeljek"
	| 2 -> "torek"
	| 3 -> "sreda"
	| 4 -> "cetrtek"
	| 5 -> "petek"
	| _ -> "vikend";;

teden 3;;

let implikacija (a,b) = match (a,b) with
| (false,false) -> true
| (false,true) -> true
| (true,false) -> false
| (true,true) -> true;;

implikacija (false,true);;

let implikacija2 (a,b) = match (a,b) with
| (true,false) -> false
| _ -> true;;

implikacija2 (false, true);;

let vecje10 x = match (x > 10) with
| true -> "je vecje"
| _ -> "ni vecje" (* karkoli druzga*)

vecje10 2;;


(* nalogo ce je crka notr v polovici abecede naredi *)

let prvaPol x = 
	if ((int_of_char x) < 65) || ((int_of_char x) < 91) 
	&& ((int_of_char x) > 97) || ((int_of_char x) > 122) then "ni crka"
	
	else if(((int_of_char x) <= 76) ||
		(((int_of_char x) >= 97) &&
	((int_of_char x) <= 108))) then "je v prvi polovici"
	
	else "ni v prvi polovici";;
	
	prvaPol '!';;   
	
	let samoglasnik x = match x with
	| 'a' | 'e' | 'i' | 'o' | 'u' -> true
	| 'A' | 'E' | 'I' | 'O' | 'U' -> true
	| _ -> false;;

	samoglasnik 'i';;




	let prvaPolPM x = match x with
	| 'a' .. 'l' | 'A' .. 'L' -> "Je v prvi polovici"
	| 'm' .. 'z' | 'M' .. 'Z' -> "ni v prvi polovici"
	| _ -> "ni crka";;

prvaPolPM '!';;

let vsotaProdukt (a, b, delaj) = match ( a, b, delaj) with
| (_,_,"vsota") -> (a+b,(a,b,delaj)) 
| (_,_,"produkt") as i -> (a*b, i)  
| __ as i -> (0, i);;

vsotaProdukt (9,8,"");;


let vsotaProduktKolic (a, b, delaj) = match ( a, b, delaj) with
| (_,_,"vsota") -> (a+b,(a,b,delaj)) 
| (_,_,"produkt") as i -> (a*b, i)  
| (_,_,"kolic") as i when (b != 0) -> (a/b, i)
| __ as i -> (0, i);;

vsotaProduktKolic (6,7,"kolic");;


let glava sez = match sez with
| [] -> failwith "hd"
| g::r -> g;;

glava [6;5;4;6;5];;

let rep sez = match sez with
| [] -> failwith "hd"
| g::r -> r;;

rep [6;5;4;6;5];;

let rep2 sez = match sez with
| [] -> failwith "tl"
| g::r -> r (* tuki jze najde*)
| g::r::r2 -> r2;;  (*das tega gor pa bo  bols*)

rep2[6;7;3;4;5];;
(*tisto fora k najde ze en if pa ce vse prevedes bo error*)


(* glava 2 funkcija dobit od koga*)

!i;;
i := !i +1; !i;;


let rec fib n = match n with
| 1 | 2 -> 1
| _ -> fib (n-1) + fib (n-2);;

fib 9;;

let rec sezFib n = match n with
| 0 -> []
| _ -> sezFib (n-1) @ [fib n];;

sezFib 9;;

let rec zmnozi sez = match sez with
| [] -> 1
| g::r -> g * zmnozi r;;

zmnozi (sezFib 20);;

(* je isto*)
(* 6;7;3*)
(* 6::7;3*)
(* 6::7::3 itd*)

let rec obrni sez = match sez with
| [] -> []
| g::r -> obrni r @ [g];;

obrni (sezFib 10);;





(* 9. Napiši funkcijo, ki niz:string prepiše v seznam:char list *)
let explode s =
 let rec explode2 i l =
  if (i = 0) then (s.[i] :: l)
  else explode2 (i-1) (s.[i] :: l)
 in
 explode2 ((String.length s) -1) [];;
(*banana v seznam b,a,n,a,n,a , so dve funkciji, s je string.gre od zadi naprej da normalnp fila seznam?*)
(* i je integer k kaze na seznam kje se nahajamo*)
(* l je pa seznam ki ga polnemo, l je nek list k ga bomo napolnili*)
(* ce je i manjsi od nic bo vrnu l oz vse kar sem napolnl, v glavo pa vsakic dodam iz stringa s kar je na item mestu *)
(* *)
(* s je kao ana, ecplode2 zazene z parametri, se prav i je 2 in l je prazen seznam na zacetku*)
(* aktiviram notrajno funckijo, lokalno deklarirano*)
(* ker i ni manjsi od 0, poklice explode2  se prav pol je  l je a v prvem ciklu mamo enko pa a. drugi cikel, else exp2 je 0*)
(* i je 1 oz n, l je pa an in vse skupi je na se pravi 0 pa na*)
(* se vedno ni manjse od 0 se prav bo else -1, in na 0 je a in l je na, *)
(* potem doda in je potem vse ksupaj ana*)
(* se pravi ex2-1 bo imel list 'a''n''a'*)
(* ker bo i manjsi od nic bo izpisalo a *)
(* *)
(* se pravi lahko tudi popravimo da ko je enako 0, in   *)

let string = "ban ana";;
string.[6];;
explode string;;

 
(* 10. Napiši funkcijo, ki seznam:char list združi v niz:string *)
let implode s =
 let izpis = String.create (List.length s) in
 let rec implode2 i s = match s with
  | [] -> izpis
  | g::r -> izpis.[i] <- g; implode2 (i+1) r
 in
 implode2 0 s;;


let nov = String.create 7;;

nov.[0] <-  '5';;

nov;;

let seznam = ['c';'s';'t'];;
let izpis = Bytes.create (List.length seznam);;  (*nule so baje prazen string*)



(*ena izmed resitev*)
let vsota seznam = 
	let rec sestej sez = match sez with 
	| [] -> 0
	| g::r -> g + sestej r

in 
 (sestej (List.filter (fun x -> x mod 2 = 1) seznam),
 sestej (List.filter (fun x -> x mod 2 = 0) seznam) );;

vsota[6;7;4;2;3;5];;

(*z referencami?*)
let vsota sez =
	let liho = ref 0 in 
	let sodo = ref 0 in
	let rec sestej s = match s with
	| [] -> (!liho,  !sodo)
	| g::r when (g mod 2 = 0) -> sodo := !sodo + g; sestej r
	| g::r -> liho := !liho +g; sestej r  (*je drugace ker drugega kot sodo ne more bit*)
	in sestej sez;;

	vsota[6;7;4;2;3;5];; 
	
	in (!liho,  !sodo)
	
	



