(*Naloga1:*)
(* Dana je funkcija fib3, ki je definirana na sledeč način:
fib3(n) = 1, za  n=1,2,3
fib3(n) = fib3(n-1)+fib3(n-2)+fib3(n-3), za n>3
Napiši funkcijo fib3list : int -> int list, ki generira seznam vrednosti 
funkcije fib3 od 1 do n, kjer je n>0 parameter funkcije. 
Primer:
# fib3 6;;
_ : int list = [1; 1; 1; 3; 5; 9]
in uporabis da das dve funkciji skupaj*)

let rec fib n = match n with
| 1 | 2 | 3 -> 1
| _ -> fib (n-1) + fib (n-2) + fib(n-3);;
fib 9;;
in
let rec sezFib n = match n with
| 0 -> []
| _ -> sezFib (n-1) @ [fib n];;
sezFib 6;;
//karamarja vaja
//da se lepse zapakira

let rec fib3 n = match n with
| _ when (n <= 3) -> 1
| _ -> fib3 (n-1) + fib3 (n-2) + fib3 (n-3);;

fib3 6;;

let rec fib3list n = match n with
| 0 -> []
| _ -> fib3list (n-1) @ [fib3 n];;

fib3list 6;;

//sestavi n ni n v fib3n
let sestavi n = 
	let rec fib3 x = match x with
	| _ when (x <= 3) -> 1
	| _ -> fib3 (x-1) + fib3 (x-2) + fib3 (x-3)
	in
	let rec fib3list y = match y with
	| 0 -> []
	| _ -> fib3list (y-1) @ [fib3 y]
	in fib3list n;; 

sestavi 9;;
//fib3list kavtiviras z zadnjim inom 
(**)
(* Naloga2:
Dan je seznam parov, ki vsebujejo ključ tipa 'k in vrednost tipa 'v
Napiši funkcijo filter ('v->bool) -> 'k*'v list -> 'k list, 
ki iz seznama parov določenim z 2. parametrom izbere tiste kluče za katere vrne 
funkcija ('v->bool) določena s 1. parameterom vrednost true. Rezultat funcije filter je 
seznam takšnih ključev. 

Primer:
# filter (function x -> x=0) [(1,0);(2,1);(3,0);(4,1)];;
- : int list = [1; 3]
kljuci so lahko katerikoli tipi, so lahko tudi isti
pogledal bo kaj je na drugem delu seznama in bo tisto kar je zraven skopiralo ven
npr fun x -> x < 'l' pa bo npr danes,a je,t sonce,b in bo izpisal danes sonce*)
let rec filter funk sez =
filter (function x -> x=0) [(1,0);(2,1);(3,0);(4,1)];;
//namig  g::r , fstr pa scnd nardis na nterico k je fst pa v je snd od glave
//kramar
//rec ker je seznam
let rec filter f sez = match sez with
| [] -> []
| g::r when (f (snd g)) = true -> (fst g) :: filter f r   (*lahko tudi g::r -> snd g al neki)*)
| g::r -> filter f r;;
(* druga resitev*)
let rec filter f sez = match sez with
| [] -> []
| (k,v)::r when (f v) = true -> k :: filter f r   (*lahko tudi g::r -> snd g al neki)*)
| g::r -> filter f r;;
filter (function x -> x=0) [(1,0);(2,1);(3,0);(4,1)];;
(**)
(* *)
(* Naloga3:
Napiši funkcijo cikli : int -> int -> int list, 
ki za klic cikli m n generira 
seznam n ciklov števil od 0  do m-1.
 @ je spajanje seznamov, :: pa dodajanje
Primer:
# cikli 3 4;; 
- : int list = [0; 1; 2; 0; 1; 2; 0; 1; 2; 0; 1; 2]
*)let rec cikl m n = match n with
| 0 -> []
| _ ->  
	;;
let cikli m n = 
	let rec cikelM m = match m with
	| 0 -> []
	| _ -> cikelM (m-1) @ [m-1]
	in
	let rec cikelN n = match n with
	| 0 -> []
	| _ -> cikelN (n-1) @ cikelM m
	in
	cikelN n;;
cikli 100 10;; 
//posebi funkcije
	let rec cikelM m = match m with
	| 0 -> []
	| _ -> cikelM (m-1) @ [m-1];
	
	let rec cikelN m n= match n with
	| 0 -> []
	| _ -> cikelN m (n-1) @ cikelM m;
	
	let cikli m n = cikelN m n;;

cikli 3 4;;
let cikliFor m n =
	let sez = ref [] in
	for i = 1 to n do
		for j = 0 to (m-1) do
			sez := !sez @ [j]
			done
		done;
	!sez;;

cikliFor 3 4;;