(*Slika neke računalniške naprave je definirana z naslednjim tipom type 
slika = array array int, kjer so pike, ki sestavljajo sliko predstaljene 
z 0 ali 1. Pika je osvetljena, če ima vrednost 1. 
Napiši funkcijo xor : slika -> slika -> slika, ki dve slike kombinira tako, 
da naredi operacijo xor nad 
istoležnimi pikami. Predpostavimo, da so vhodne slike istih dimenzij.*)

type slika = {a:int array array};;

let xor s1 s2 = 
	for i = 0 to ((Array.length s1.a)-1) do
		for j = 0 to ((Array.length s1.a.(i)) - 1) do
			s1.a.(i).(j) <- s1.a.(i).(j) lxor s2.a.(i).(j)
		done
	done;s1;;


let slika1 = {a=[|[|1;2;3|]|]};;
let slika2 = {a=[|[|1;2;3|]|]};;

xor slika1 slika2;;


(*2. naloga (25%) Napiši funkcijo višjega reda skrci : 
(int * int -> int) -> list int -> int, ki aplicira binarno funkcijo 
(1.parameter) na seznamu celih števil 
2.parameter) na naslednji način. Naj bo f funkcija, ki predstavlja 1. paremeter, 
in l = [i1; i2; ...; in] seznam, ki predstavlja 2. parameter. skrci f l = f i1 (f i2 f(...(f in-1 in))) 
Predpostavi, da ima seznam l vsaj dva elementa!
*)
let sez = [1;2;3;4;5;6];;

let fuc = (fun (x,y) -> x*y);;

let rec skrci f sez = match sez with
| [] -> 0
| g1::g2::r -> (f g1 g2);(skrci f r) ;;


skrci fuc sez;;

(*original, mogoce delujoca*)
let rec skrci f sez = match sez with
| [] -> 0
| g1::g2::r -> skrci f r ;(f g1 g2);;


(*3. naloga (25%) Izrazi zelo enostavnega jezika L so sestavljeni iz vrednosti med*)
(*  katerimi so postavljene operaciji PLUS ali KRAT. Primer izraza je: 1 PLUS 2 KRAT 3,*)
(*  kar ustreza aritmetičnem izrazu 1 + 2 * 3. Izraze jezika L lahko definiramo z *)
(* naslednjimi tipi: *)
(* # type operacija = PLUS | MINUS;; type operacija = PLUS | *)
(* MINUS # type element = Vr of int | Op of operacija;; type element = Vr of int | *)
(* Op of oper acija # type izraz = list element;; type izraz = list element *)
(* Napiši funkcijo preveri : izraz -> bool, ki preveri ali je izraz pravilno napisan.*)

type operacija = PLUS | MINUS | KRAT;;
type element = Vr of int | Op of operacija;;
type izraz = element list;;
let enacba = []

let iz1 = [Vr 1;Op PLUS;Vr 1;Op KRAT;Vr 3];;

let rec preveri izraz = match izraz with
| (Vr _)::[] -> true
| (Op _)::[] -> false
| (Vr _)::(Op _)::r -> true && preveri r
| g1::g2::r -> false && preveri r
| [] -> false;;

(*Binarno drevo je definirano s tipom
type bdrevo = List of int | Drevo of bdrevo * int * bdrevo,
ki ima vrednosti v listih definirane kot List(v), kjer je v celo število. Notranja vozlišča
so definirana kot trojica Drevo(l, v, d), kjer sta l in d levo in desno poddrevo, v pa
je vrednost, ki je začetno 0 za vsa notranja vozlišča.
a) (25%) Napiši funkcijo prestej : bdrevo -> bdrevo, ki drugo komponento vseh
notranjih vozlišč izhodnega drevesa zamenja z vsoto vseh listov poddrevesa.
b) (15%) Napiši funkcijo prestej : bdrevo -> int, ki sešteje vrednosti podane v
listih vhodnega drevesa. *)

type bdrevo = List of int | Drevo of bdrevo * int * bdrevo;;

(*a)*)
let vrni_v x = match x with
| List x -> x
| Drevo (lpd,v,dpd)->v;;

let rec prestej drevo = match drevo with
| List x -> List x
| Drevo (lpd,v,dpd) -> Drevo(prestej lpd,
												vrni_v(prestej lpd) + vrni_v(prestej dpd)
												,prestej dpd );;

(*b)*)

let rec prestej drevo = match drevo with
| List x -> x
| Drevo (lpd,0,dpd) -> prestej lpd + prestej dpd;;


