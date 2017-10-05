(*VAJE ZA IZPIT*)
(** Hello world! **)

print_string "Hello world!\n";;

(** osnove **)

let a = 4;;
let b = 3.;;
(float_of_int a) +. b;;
(* - : float = 7. *)

2. ** 3.;;
(* - : float = 8. *)

int_of_char 'a';;
(* - : int = 97 *)

char_of_int 88;;
(* - : char = 'X' *)

string_of_int 2843;;
(* - : string = "2843" *)

(** n-terice **)

("a", 'a', int_of_char 'a', true);;
(* - : string * char * int * bool = ("a", 'a', 97, true) *)

let n = (1, "2");;
fst n;;
(* - : int = 1 *)
snd n;;
(* - : string = "2" *)

(** lokalne deklaracije spremenljivk **)

let dan = "Danes je " in dan ^ "petek";;
(* - : string = "Danes je petek" *)

let x = 1+1 in let y = 2+2 in  x+y;;
(* - : int = 6 *)

(** funkcije **)

fun (a, b) -> (a+1, a*b);;
(* - : int * int -> int * int = <fun> *)

(fun (a, b) -> (a+1, a*b)) (3, 4);;
(* - : int * int = (4, 12) *)

let neki = fun (a, b) -> (a+1, a*b);;
(* val neki : int * int -> int * int = <fun> *)

neki (3, 4);;
(* - : int * int = (4, 12) *)

fun x y -> (x+y, x*y);;
(* - : int -> int -> int * int = <fun> *)
fun x -> fun y -> (x+y, x*y);;
(* - : int -> int -> int * int = <fun> *)

let sestej = fun x y -> x+y;;
let pristej1 = fun x -> sestej 1 x;;
(* val sestej : int -> int -> int = <fun>
   val pristej1 : int -> int = <fun> *)
sestej 2 3;;
pristej1 7;;
(* - : int = 5
   - : int = 8 *)

(** ujemanje vzorcev **)

let vecje10 x = match (x > 10) with
| true -> "je vecje"
| false -> "ni vecje";;

let je_ab ch = match ch with
| 'a' | 'b' -> true
| _ -> false;;

let vsotaAliprodukt (a, b, kaj) = match (a, b, kaj) with
| (_, _, "vsota") -> (a+b, (a, b, kaj))
| (_, _, "produkt") as parameter -> (a*b, parameter)
| _ as parameter -> (0, parameter);;
(* val vsotaAliprodukt : int * int * string -> int * (int * int * string) = <fun> *)

vsotaAliprodukt (1, 3, "vsota");;
(* - : int * (int * int * string) = (4, (1, 3, "vsota")) *)

let rec fib n = match n with
| 1 -> 0
| 2 -> 1
| _ -> fib (n-1) + fib (n-2);;

let rec fibSez n = match n with
| 1 -> [0]
| _ -> [fib n]@(fibSez (n-1));;

fibSez 7;;
(* - : int list = [8; 5; 3; 2; 1; 1; 0] *)

(* char list to string *)
let implode s =
 let izpis = String.create (List.length s) in
 let rec implode2 i s = match s with
  | [] -> izpis
  | g::r -> izpis.[i] <- g; implode2 (i+1) r
 in
 implode2 0 s;;

(* string to char list *)
let explode s =
 let rec explode2 i l =
  if (i < 0) then l
  else explode2 (i-1) (s.[i] :: l)
 in
 explode2 ((String.length s) -1) [];;

(* Napisi funkcijo, ki preveri ali je podani seznam palindrom. *)
let je_palindrom seznam =
 let rec vrni seznam n = match seznam with 
 | [] -> 0
 | _ when (n<=0) -> 0
 | g::r when (n=1) -> g
 | g::r -> vrni r (n-1) in
 let dolzina = List.length seznam in
 let rec primerjaj i j = match i with
  | _ when (i = j) -> true
  | _ when ((vrni seznam i) != (vrni seznam j)) -> false
  | _ -> primerjaj (i+1) (j-1)
 in
 primerjaj 1 dolzina;;

let je_palindrom2 seznam =
 let rec obrni seznam = match seznam with
  | [] -> []
  | g::r -> (obrni r) @ [g]
 in
 (seznam = obrni seznam);;


(* Napisi funkcijo, ki preveri ali je podani niz palindrom. *)
let je_palindromS niz =
	let seznam = (explode niz) in
 		let rec obrni seznam = match seznam with
    | [] -> []
    | g::r -> (obrni r) @ [g]
    in
    (seznam = obrni seznam);;

(* vsi podnizi danega niza *)

let rec obrni seznam = match seznam with
 | [] -> []
 | g::r -> (obrni r) @ [g]

let rec podnizi s =
	let ch = explode s in
	let rec podnizi2 c = match c with
	| [] -> ()
	| sez -> print_string (implode (sez) ^ ", "); podnizi2 (obrni (List.tl (obrni (sez))))
	in
	podnizi2 ch;
	podnizi (implode (List.tl ch));;
(* val podnizi : bytes -> 'a = <fun> *)
podnizi "banana";;

(* izpisovanje *)
let rec try1 s = match s with
| [] -> []
| g::r -> g::try1 r;;

let rec try2 s = match s with
| [] -> []
| g::r -> [g]@try2 r;;

let rec try3 s = match s with
| [] -> []
| g::r -> try3 r@[g];;

let sez = [1;2;3;4;5;6;7;8;9];;
try1 sez;;
try2 sez;;
try3 sez;;
(*- : int list = [1; 2; 3; 4; 5; 6; 7; 8; 9]
  - : int list = [1; 2; 3; 4; 5; 6; 7; 8; 9]
  - : int list = [9; 8; 7; 6; 5; 4; 3; 2; 1] *)

(* obseg trikotnika *)

let rec obseg nt = match nt with
| (a,b,c) when (a+b < c || a+c < b || b+c < a) -> 0
| (a,b,c) -> a+b+c;;
(* val obseg : int * int * int -> int = <fun> *)

(* seznam v 2-terico, na levi vsota lihih, na desni vsota sodih st *)
let vsotaSodeLihe se = 
	let rec vsota sez l s = match sez with
	| [] -> (l, s)
	| g::r when (g mod 2 = 1) -> vsota r (l+g) s
	| g::r -> vsota r l (s+g)
	in
	vsota se 0 0;;

(* dva seznama, najprej en element iz prvega pol dva iz drugega itd.*)
(* ce se en izprazne zapolne z drugim *)
	
	
let rec insert sez1 sez2 i = match (sez1, sez2) with
| ([], []) -> []
| ([], g::r) -> [g]@insert sez1 r (i+1)
| (g::r, []) -> [g]@insert r sez2 (i+1)
| (g1::r1, g2::r2) when (i mod 3 = 0) -> [g1]@insert r1 sez2 (i+1)
| (g1::r1, g2::r2) -> [g2]@insert sez1 r2 (i+1);;

insert sez1 sez2 0;;

(* preverjanje gnezdenja oklepajev *)

let preveriOkl s =
	let sez = explode s
	in
	let rec preveri sez c = match sez with
	| [] when (c > 0) -> false
	| [] -> true
	| g::r when (g = ')' && c = 0) -> false
	| g::r when (g = '(') -> preveri r (c+1)
	| g::r -> preveri r (c-1)
	in
	preveri sez 0;;

(* niz "a+b+" *)

let aliVsebuje s =
	let sez = explode s
	in
	let rec check sez = match sez with
	| [] -> false
	| g1::g2::r when (g1 = 'a' && g2 = 'b') -> true
	| g1::g2::r when (g2 = 'a') -> check ([g2]@r)
	| g1::g2::r -> check r
	| g::r -> false
	in
	check sez;;


(* podnizi nizov *)
let max a b c =
	if(a > b && a > c) then a
	else if (b > c && b > a) then b
	else c;;
let a = "ASd";;
let arr = Array.make_matrix (5) (5) 2

arr.(4).(3) = max (arr.(1).(1)) (arr.(0).(1)) (arr.(1).(0)) +1

let najdiPodnize n1 n2=
	let arr = Array.make_matrix (String.length(n1)+1) (String.length(n2)+1) 0
	in
	for i = 1 to (String.length(n1))
	do
		for j = 1 to (String.length(n2))
		do
			if(n1.[i-1] = n2.[j-1]) then arr.(i).(j) <- ((max (arr.(i-1).(j)) (arr.(i).(j-1)) (arr.(i-1).(j-1))) +1)
			else arr.(i).(j) <- max (arr.(i-1).(j)) (arr.(i).(j-1)) (arr.(i-1).(j-1))
		done
	done;
	arr;;

najdiPodnize "mmmaa" "matematika";;
najdiPodnize "seno" "sosednost";;

(** fib modified **)
let rec fib n = match n with
| a when a <= 3 -> 1
| a -> fib (a-1) + fib (a-2) + fib (a-3);;

fib 8;;

let fib3List n = 
	let rec fib3 n = match n with
	| a when a <= 3 -> 1
	| a -> fib (a-1) + fib (a-2) + fib (a-3)
	in
	let rec list n = match n with
	| 1 -> [1]
	| 2 -> [1;1]
	| 3 -> [1;1;1]
	| a -> list (n-1)@[fib3 a]
	in
	list n;;

fib3List 10;;
(* - : int list = [1; 1; 1; 3; 5; 9; 17; 31; 57; 105] *)

(** a v 1,2,3, odvisno kolko jih je, b v 0**)

let c =  ['a'; 'a'; 'a'; 'a'; 'a'; 'b'; 'a'; 'a'];;

let rec pretvori sez i = match sez with
| [] when (i = 0) -> []
| [] -> [i]@pretvori [] 0
| g::r when (g != 'a') -> [i]@[0]@pretvori r 0
| g::r when (g = 'a' && i < 3) -> pretvori r (i+1)
| g::r -> [i]@pretvori r 0;;

pretvori c 0;;
(* - : int list = [3; 1; 0; 2] *)

(** TIPI **)

(** prestej st tipov v seznamu **)

type geo_objekt = Tocka | Premica | Krog | Trikotnik;;

let gl = [Tocka;Tocka;Premica;Krog;Trikotnik;Krog;Krog];;

let rec prestej sez go = match sez with
| [] -> 0
| g::r when g=go -> 1+prestej r go
| g::r -> prestej r go;;

(* val prestej : 'a list -> 'a -> int = <fun> *)

(* dvoterica vozlic v dvoterico seznamov *)

type ( 'a , 'b ) drevo =
| Prazno
| Vozliscea of 'a * ('a , 'b) drevo
| Vozlisceb of 'b * ('a , 'b) drevo;;

let dr1 = Vozliscea (9, Vozlisceb ("je", Vozlisceb("bo", Prazno)));;

let razcepi drevo = 
	let a = ref [] in
	let b = ref [] in
	let rec raz dr = match dr with
	| Prazno -> (!a, !b)
	| Vozliscea (x, y) -> a := !a@[x]; raz y
	| Vozlisceb (x, y) -> b := !b@[x]; raz y
	in
	raz drevo;;
(* val razcepi : ('a, 'b) drevo -> 'a list * 'b list = <fun> *)
razcepi dr1;;
(* ([9],["je";"bo"]) *)

(* st in operacija, izracunaj koncni rez *)

type  izraz = 
	| Nil
	| Stevilo  of int * izraz
	| Oper of char * izraz;;

let ovrednoti e =
	let st = ref 0 in
	let op = ref '+' in
	let rec racunaj e = match e with
	| Nil -> !st
	| Stevilo (x, y) when (!op = '+') -> st := !st + x; racunaj y
	| Stevilo (x, y) -> st := !st - x; racunaj y
	| Oper (x, y) -> op := x; racunaj y
	in
	racunaj e;;
(* val ovrednoti : izraz -> int = <fun> *)

let e = Stevilo(-5, Oper('+', Stevilo(0,Nil)));;

ovrednoti e;;


(* izpisi liste poddrevesa za katere f vrne true *)

type 'a bindrevo = List of 'a | Drevo of 'a bindrevo * 'a bindrevo;;

let dr = Drevo(Drevo(List "4",List "6"), Drevo(List "8",List "1"));;

let rec izpis dr f = match dr with
| Drevo(List a, List b) when (f a && f b) -> a; b
| Drevo(List a, List b) when f a -> a
| Drevo(List a, List b) when f b -> b
| Drevo(Drevo (a,b), List c) when f c -> izpis a f;izpis b f; c
| Drevo(Drevo (a,b), List c) -> izpis a f; izpis b f
| Drevo(List a, Drevo (b,c)) when f a -> a; izpis b f; izpis c f
| Drevo(List a, Drevo (b,c)) -> izpis b f; izpis c f
| Drevo(Drevo (a,b), Drevo (c,d)) -> izpis a f; izpis b f; izpis c f; izpis d f;;

let rec izpis dr f = match dr with
| List a when f a -> print_string  a
| List a -> ()
| Drevo (a,b) -> izpis a f; izpis b f;;

izpis dr (fun x -> x="6");;

(* dolzine vej *)

type 'a grm = Nic | Ena of 'a * 'a grm | Dva of 'a grm * 'a * 'a grm;;

let dolzineVej grm =	
  let rec dolzine grm n = match grm with
  | Nic -> print_string "dolzina veje : "; print_int n; print_string "\n"
  | Ena(a, b) -> dolzine b (n+1)
  | Dva(a, b, c) -> dolzine a (n+1); dolzine c (n+1)
	in
	dolzine grm 0;;
(* val dolzineVej : 'a grm -> unit = <fun> *)

let grm = Dva(Ena(1,Dva(Nic,1,Nic)),1, Dva(Ena(1, Nic),1, Nic));;

(* linked list in izpis dolzine*)

type 'a seznam = Empty | Next of 'a * 'a seznam;;

let sez = (Next(2,Next(4,Next(6,Next(8,Next(10,Next(12,Empty)))))));;

let rec dolzina sez = match sez with
| Empty -> 0
| Next(a,b) -> 1+dolzina b;;

(* val dolzina : 'a seznam -> int = <fun> *)

(* vsota poddreves *)

type itree = Nil | Node of itree*int*itree;;

let d = Node(Node(Nil,3,Nil),5,Node(Nil,2,Node(Nil,1,Nil)));;

let sumsub drevo = 
  let rec vrednost voz = match voz with
  | Nil -> 0
  | Node(l, v, r) -> v
	in
  let rec sumsub1 drevo = match drevo with
  | Nil -> Nil
  | Node(l, v, r) -> Node(sumsub1 l, v+vrednost (sumsub1 l)+ vrednost (sumsub1 r),sumsub1 r)
	in
	sumsub1 drevo;;

sumsub d;;
(* - : itree = Node (Node (Nil, 3, Nil), 11, Node (Nil, 3, Node (Nil, 1, Nil))) *)

(* cikli *)

let cikli m n =
	let rec cikli2 m = match m with
	| 1 -> [0]
	| _ -> cikli2 (m-1)@[m-1]
	in
	let rec cikli1 m n = match n with
	| 0 -> []
	| _ -> cikli2 m@cikli1 m (n-1)
	in cikli1 m n


let cikli m n = 
	let sez = ref [] in
	for i = 1 to n do
		for j = 0 to (m-1) do
			sez := !sez @ [j]
		done
	done; !sez;;

(* repp *)

let repp f n = (fun x -> for i = 1 to n do f x done; x);;

let rec repp f n x = match n with
| 0 -> x
| _ -> repp f (n-1) (f x);;
(* val repp : ('a -> 'a) -> int -> 'a -> 'a = <fun> *)

repp (fun x -> x+1) 5 4;;

(* definiraj tip in preveri izomorfizem *)

type 'a btree = Leaf | Node of 'a btree * 'a * 'a btree ;;
let d2 = Node(Leaf, "sa", Leaf);;
let izomorf d1 d2 =
	let b = ref true
	in
  let rec preveri d1 d2 = match (d1, d2) with
	| (Leaf, Leaf) -> ()
  | (Leaf, Node(l,c,r)) -> b := false
  | (Node(l,c,r), Leaf) -> b := false
  | (Node(l1, c1, r1), Node(l2, c2, r2)) -> preveri l1 l2; preveri r1 r2
	in
	preveri d1 d2; !b;;

(* val izomorf : 'a btree -> 'b btree -> bool = <fun> *)

let d1 = (Node(Node(Leaf, 4, Node(Leaf, 5, Leaf)), 6, Leaf));;
let d2 = (Node(Node(Leaf, 4, Node(Leaf, 5, Leaf)), 6, Leaf));;

izomorf d1 d2;;

(* class matrika, set, get *)

class matrika m n =
	object (self)
	val mutable mat = Array.make_matrix m n 0
	method get i j = mat.(i).(j)
	method set i j n = mat.(i).(j) <- n
	method getM = m
	method getN = n
	method getMat = mat
	method setMat m = mat <- m
	method mul mat2 =
		let tmpMat = Array.make_matrix (Array.length(mat)) (Array.length(mat2.(0))) 0
		in
		for i = 0 to Array.length(mat)-1 do
			for j = 0 to Array.length(mat)-1 do
				let tmp = ref 0
				in
				for k = 0 to Array.length(mat2)-1 do
					tmp := !tmp + (mat.(j).(k) * mat2.(k).(i));
					done;
				tmpMat.(i).(j) <- !tmp
			done
		done;
		self#setMat tmpMat
end;;

let m1 = new matrika 2 3;;
m1#set 0 0 1;;
m1#set 0 1 2;;
m1#set 0 2 3;;
m1#set 1 0 4;;
m1#set 1 1 5;;
m1#set 1 2 6;;

let m2 = new matrika 3 2;;
m2#set 0 0 1;;
m2#set 1 0 2;;
m2#set 2 0 3;;
m2#set 0 1 4;;
m2#set 1 1 5;;
m2#set 2 1 6;;

m1#mul m2#getMat;;

m1#getMat;;

(* izbrisi zaporedne ponovitve *)

let rec popravi sez = match sez with
| [] -> []
| g1::g2::r when (g1 = g2) -> popravi ([g1]@r)
| g1::g2::r -> g1::popravi ([g2]@r)
| g::r -> [g];;
let s1 = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
let sez = popravi s1;;
sez;;

(* shrani zaporedne ponovitve v podsezname *)

let popravi2 sez =
	let rec naredi seznam tmp = match seznam with
	| [] -> []
	| g1::g2::r when g1 = g2 -> naredi ([g2]@r) ([g1]@tmp)
	| g1::g2::r -> ([g1]@tmp)::(naredi ([g2]@r) [])
	| g::r -> ([g]@tmp)::(naredi r [])
	in
	naredi sez [];;

popravi2 ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e"];;

(* izpisi seznam fst katerih snd vrne true za fun v *)

let rec filter v k = match k with
| [] -> []
| (a,b)::r when v b ->a::filter v r
| g::r -> filter v r;;
(* val filter : ('a -> bool) -> ('b * 'a) list -> 'b list = <fun>	 *)

filter (function x -> x=0) [(1,0);(2,1);(3,0);(4,1)];;

(* apliciraj fun na vse elemente drevesa *)

type 'a struc = |Elm of 'a |Pair of 'a struc * 'a struc |Triple of 'a struc*'a struc*'a struc

let rec smap dr f = match dr with
| Elm a -> Elm (f a)
| Pair(a,b) -> Pair(smap a f, smap b f)
| Triple(a,b,c) -> Triple(smap a f, smap b f, smap c f);;

smap (Triple(Pair(Elm 1,Elm 2),Elm 3,Elm 4)) (fun x->x+1);;

(* razred slika, stance na veckratnik st 100, zrcaljenje preko osi x in y *)

class slika m n =
	object(self)
	val mutable slika = Array.make_matrix m n 0
	val mutable m = (m:int)
	val mutable n = (n:int)
	initializer
		let tmp1 = m mod 100 and tmp2 = n mod 100 in
		m <- m + (100 - tmp1);
		n <- n + (100 - tmp2);
		slika <- Array.make_matrix m n 0
	method getM = m
	method getN = n
	method zrcaliX =
		for i = 0 to self#getM/2-1 do
			for j = 0 to self#getN-1 do
				let tmp = slika.(i).(j)
				in (slika.(i).(j) <- slika.(self#getM-i-1).(j); slika.(self#getM-i-1).(j) <- tmp)
			done
		done
end;;

(* izpisi tretji nivo ce obstaja *)
type 'a drevo = {
mutable levo : 'a bin_drevo;
mutable vozlisce: 'a;
mutable desno : 'a bin_drevo
}
and 'a bin_drevo = Prazen | Vozlisce of 'a drevo;;
let izpis dr =
  let rec izpisi dr i = match dr with
  | Prazen -> []
  | Vozlisce{levo;vozlisce;desno} when i = 3 -> [vozlisce]
  | Vozlisce{levo;vozlisce;desno} -> (izpisi levo (i+1))@(izpisi desno (i+1))
	in
	izpisi dr 1;;

let d = Vozlisce {levo=Prazen; vozlisce=5; desno=Vozlisce {levo=Prazen; vozlisce=2; desno=Vozlisce{levo=Prazen; vozlisce=10; desno=Prazen}}};;

izpis d;;

(* kombinacije k st izmed n stevil *)

let kombi k sez =
	

(* 1. DN *)

let chars = ['A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z';'1';'2';'3';'4';'5';'6';'7';'8';'9';'0'];;
let mors = [".-";"-...";"-.-.";"-..";".";"..-.";"--.";"....";"..";".---";"-.-";".-..";"--";"-.";"---";".--.";"--.-";".-.";"...";"-";"..-";"...-";".--";"-..-";"-.--";"--..";".----";"..---";"...--";"....-";".....";"-....";"--...";"---..";"----.";"-----"];;

let caps ch = 
	if(int_of_char ch > 96 && int_of_char ch < 123) then char_of_int(int_of_char ch - 32)
	else ch;;

caps 'a';;


let rec morse2 c s1 s2 = match s1 with
| [] -> ""
| g::r when (caps c = g) -> List.hd(s2)
| g::r -> morse2 c r (List.tl(s2));;

let morse ch = morse2 ch chars mors;;


morse 'o';;

let stringAllCaps s =
 let rec explode2 i l k=
  if (i < 0) then l::k
	else if (s.[i] = ' ' && List.length l = 0) then explode2 (i-1) l k
  else if (s.[i] = ' ' && i != 0 && s.[i-1] != ' ') then explode2 (i-1) [] (l::k)
	else if (s.[i] = ' ' && i != 0 && s.[i-1] = ' ') then explode2 (i-1) l k
	else if (i = 0 && s.[i] = ' ') then explode2 (i-1) l k
	else explode2 (i-1) (s.[i] :: l) k
 in
 explode2 ((String.length s) -1) [] [];;

stringAllCaps "   ban   an        a        ";;

let rec morseList2 sez = match sez with
| [] -> []
| g::r -> (morse g)::(morseList2 r);;


let rec morseList sez = match sez with
| [] -> []
| []::vr -> morseList vr
| vg::vr -> (morseList2 vg)::(morseList vr);;


morseList (stringAllCaps "a b c");;


let rec stringToMorseList s = 
	morseList (stringAllCaps s);;

stringToMorseList  "banan a sos";;

let explode s =
 let rec explode2 i l =
  if (i < 0) then l
  else explode2 (i-1) (s.[i] :: l)
 in
 explode2 ((String.length s) -1) [];;


let rec pretvori2 sez = match sez with
| [] -> []
| g::r -> (explode g)::(pretvori2 r);;


let rec pretvori sez = match sez with
| [] -> []
| vg::vr -> (pretvori2 vg)::(pretvori vr);;


pretvori ([[".-"; "-..."]; [".-"]]);;

#load "graphics.cma";;

let vZvok car = match car with
|'.' -> Graphics.sound 2000 100
|'-' -> Graphics.sound 2000 300
|'v' -> Graphics.sound 0 100
|' ' -> Graphics.sound 0 700
|_ -> Graphics.sound 0 0;;


let rec zaigraM3 sez = match sez with
| [] -> []
| g::r -> vZvok g; zaigraM3 r;;

let rec zaigraM2 sez = match sez with
| [] -> []
| []::vr -> zaigraM2 vr
| vg::vr -> (zaigraM3 vg)::(zaigraM2 vr);;


let rec zaigraM sez = match sez with
| [] -> []
| []::zvr -> zaigraM zvr
| zvg::zvr -> (zaigraM2 zvg)::(zaigraM zvr);;

zaigraM (pretvori (stringToMorseList("ertwer")));;

(* 2. DN *)

let pomnilnik = (Array.make 100 '0');;

let ar = ref [(100, 0)];;
let dolB = ref [(0,0)];;
dolB := List.tl(!dolB);;
let c = ref (-1);;

let malloc s l sez2 = 
	let sez = ref !ar in
	for i = 0 to (List.length !sez)-1 do
		if(snd(List.hd !sez) = s && (fst(List.hd !sez)-l = 0)) then ()
		else if(snd(List.hd !sez) = s && (fst(List.hd !sez)-l != 0)) then
			sez2 := !sez2@[(fst(List.hd !sez) - l, snd(List.hd !sez) + l)]
		else 	sez2 := !sez2@[(fst(List.hd !sez), snd(List.hd !sez))];
		sez := List.tl(!sez)
		done;
		ar := !sez2;;

let free p l =
	for i=p to p+l-1 do 
	pomnilnik.(i) <- '0';
	done;;

let rec repair p l sez = match sez with
| [] -> []
| g::r when (snd g > p+l) -> [(l, p)]@[g]@r
| g::r when (snd g + fst g = p && snd(List.hd r) = p+l) -> [(fst g + l + fst(List.hd r), snd g)]@List.tl(r)
| g::r when (snd g + fst g = p) -> [(fst g + l, snd g)]@r
| g::r when (snd g - l = p) -> [(fst g + l, snd g - l)]@r
| g::r when (snd g + fst g < p && snd(List.hd r) > p+l) -> [g]@[(l, p)]@r
| g::r -> [g]@(repair p l r);;

let rec repairdolB p l sez = match sez with
| [] -> []
| g::r when (fst g = l && snd g = p) -> repairdolB p l r
| g::r -> [g]@repairdolB p l r;;

let explode s =
 let rec explode2 i l =
  if (i < 0) then l
  else explode2 (i-1) (s.[i] :: l)
 in
 explode2 ((String.length s) -1) [];;

let rec set s sez = match sez with
| [] -> ()
| g::r when (snd g + String.length s >= Array.length pomnilnik) -> print_string "Pomnilnik je poln, izprazni nekaj prostora"
| g::r when (fst(g) >= String.length s) -> (c := snd(g); malloc (snd(g)) (String.length s) (ref []); dolB := !dolB@[(String.length s, !c)])
| g::r -> set s r;;

let insert bes = 
	set bes !ar;
	let ch = ref (explode bes) in
	if(!c != -1) then
		for i = !c to (String.length bes)-1+(!c) do
			pomnilnik.(i) <- List.hd(!ch);
			ch := List.tl(!ch)
			done;
			c := -1;;

let rec check ch i c = match ch with
| [] -> true
| g::r when (pomnilnik.(i+c) = g) -> check r i (c+1)
| g::r -> false;;

let rec contains l p sez = match sez with
| [] -> false
| g::r when (fst g = l && snd g = p) -> true
| g::r -> contains l p r;;

let delete bes = 
	let ch = explode bes in
	for i = 0 to (Array.length pomnilnik)-1 do
		if(check ch i 0 && contawins (String.length bes) i (!dolB)) then
			(free i (List.length ch); ar := repair i (String.length bes) (!ar); dolB := repairdolB i (String.length bes) (!dolB))
	done;;


let del n = 
	for i = snd n to (snd n + fst n - 1) do
		pomnilnik.(i) <- '0'
		done;;

let rec release n = match n with
| 0 -> ()
| _ -> (del (List.hd(!dolB)); dolB := List.tl(!dolB)); release (n-1);;


(* 3. DN *)

type 'a tree = {
	mutable levo:'a bin_tree;
	mutable vrednost:'a;
	mutable desno:'a bin_tree
	}
	and
	'a bin_tree =  Empty  | Node of 'a tree ;;


class ['a] drevo ime struc=
object (self)
val ime = (ime:string)
val mutable dr = (struc:'a bin_tree)
method izpisi = dr
method dodajVDrevo v = 
	let rec dodaj dr v = match dr with
	| Empty -> Node{levo = Empty; vrednost = v; desno = Empty}
	| Node{levo;vrednost;desno} when (vrednost < v) -> Node{levo;vrednost;desno = dodaj desno v}
	| Node{levo;vrednost;desno} when (vrednost > v) -> Node{levo = dodaj levo v;vrednost;desno}
	| Node{levo;vrednost;desno} -> Node{levo;vrednost;desno}
	in
	dr <- dodaj dr v;
method zdruziDrevesi d1 = 
	let rec zdruzi d1 = match d1 with
	| Empty -> ()
	| Node{levo;vrednost;desno} -> zdruzi levo; self#dodajVDrevo vrednost; zdruzi desno
	in
	zdruzi d1;
method drevoVSez =
	let rec pretvori dr = match dr with
	| Empty -> []
	| Node{levo;vrednost;desno} -> (vrednost::pretvori levo)@pretvori desno
	in
	pretvori dr;
method sezVDrevo sez =
	dr <- Empty;
	let s = ref sez
	in
	for i = 0 to (List.length(sez) - 1) do
		self#dodajVDrevo (List.hd(!s));
		s := List.tl(!s);
	done;
method brisiZDrevesa v =
	let sez = self#drevoVSez
	in
	let rec dodaj sez = match sez with
	| [] -> ()
	| g::r when (g != v) -> self#dodajVDrevo g; dodaj r
	| g::r -> dodaj r
	in
	dr <- Empty;
	dodaj sez;
end;;

let d = new drevo "neki" Empty;;
let d2 = new drevo "neki2" Empty;;

d#dodajVDrevo 4;;
d#zdruziDrevesi d2#dr;;
d#izpisi;;
d#brisiZDrevesa 15;;

class ['a] iskalnoDrevo ime struc =
object (self)
inherit ['a] drevo ime struc as super
method dodajVDrevo v = 
	let rec dodaj dr v = match dr with
	| Empty -> Node{levo = Empty; vrednost = v; desno = Empty}
	| Node{levo;vrednost;desno} when (vrednost < v) -> Node{levo;vrednost;desno = dodaj desno v}
	| Node{levo;vrednost;desno} when (vrednost > v) -> Node{levo = dodaj levo v;vrednost;desno}
	| Node{levo;vrednost;desno} -> Node{levo;vrednost;desno}
	in
	dr <- dodaj dr v;
end;;


(**
po vstavljanju vsakega elementa bi morali preveriti, ce je slucajno za dve veji nizje od kaksnega drugega elementa
v tem primeru naredimo rotacijo 

	a								b													a							c
		b				->	a		c													b			->	a		b
			c																			 c

to bi bilo najlazje narediti tako da bi dodali v tip se pionter na oceta in ko dosezemo tako vozlisce samo se premaknemo gor
in popravimo otoke.

**)

(* 1. DN LANI *)

(** VDN1 **)

(** se opravicujem za txt datoteko samo mi ni  hotlo copirat .ml ker jo imam shranjeno nekje na zascitenem predelu linuxa in se zdej ne znajdem kako to povlect vn in nimam drugega racunalnika z ocamlom da bi lahko to popravu. **)

let moja_matrika1 = [[1;2]; [3;4]];;
let moja_matrika2 = [[4;7;8]; [3;2;1]];;
(** 1. naloga **)
let rec ali_vsebuje2 mat x = match mat with 
| [] -> false
| g::r -> if (g=x) then true else ali_vsebuje2 r x;;
(** ali_vsebuje2 prejme int list pregleda ali je glava enaka iskanemu stevilu x
		in ce je vrne true, ce ne pa se klice spet sama sebe z repom od lista dokler
		ne ostane list prazen in pol vrne false **)
let rec ali_vsebuje mat x = match mat with
| [] -> false
| g::r -> if(ali_vsebuje2 g x) then true else ali_vsebuje r x;;
(** ali_vsebuje prejme int list list in klice glavo ki je int list s funkcijo
		ali_vsebuje2, ki vrne true ali false in ce vrne true izpise true cene pa
		klice spet samo sebe z repom dokler ne ostane prazna in pol vrne false **)
ali_vsebuje moja_matrika1 4;;
ali_vsebuje moja_matrika2 4;;

(** 2. naloga **)

let rec transponiraj mat = match mat with
	| [] -> [] 
	| []::rv -> transponiraj rv
	| (g::rm)::rv -> (g::List.map List.hd rv)::transponiraj (rm::List.map List.tl rv);;
(** funkcija transponiraj vzame glavo od glave od matrike [[g::rm]::rv], kjer je rm mali rep
		notranjega lista in rv je znanjega list (stolpci matrike) in glavo vstavi v list kateri 
		pobere se vse ostale glave od repa (rv) (g::List.map List.hd rv) List.map gre skozi cel 
		rv in zdruzi vse glave listov in jim vstavi se glavo od glave od rv. to nastane en list 
		ki ga vstavi v list transponiraj ki je recurzivna funkcija ki klice kateri vsebuje rm
		vstavljen v list od vseh ostali repov od rv in pol ponavlja vse to dokler ne zmanjka
		elementov v glavi od lista potem klice rep ki je tudi prazen in to ponavlja dokler ne
		zmanjka elementov v listu in potem ostane samo prazen list in takrat se ustavi **)
transponiraj moja_matrika2;;

(** 3. naloga **)

let rec izpisi_stolpec2 sez x = match sez with
| [] -> raise (Failure "prazen seznam")
| g::r -> if x = 1 then g else izpisi_stolpec2 r (x-1);;
(** funkcija izpisi_stolpec2 prejme seznam int list in stolpec ki ga zelimo izpisat in ce je
		seznam prazen vrne error "prazen seznam" cene pa vzame glavo in rep od seznama in pogleda
		ce je stolpec ki ga iscemo (x) enak 1 izpise glavo cene poklice samo sebe z repom in x-1
		tako se seznam zamakne za 1 in zmanjsa stolpec ki ga iscemo za 1 dokler ne pridemo do 
		stolpca ki ga iscemo in ga vrne spodaj klicani funkciji oziroma se seznam izprazne in
		vrne error **)
let rec izpisi_stolpec mat x= match mat with
| [] -> []
| []::rv -> izpisi_stolpec rv x
| gv::rv -> izpisi_stolpec2 gv x::izpisi_stolpec rv x;;
(** izpisi_stolpec vzame matriko in stolpec ki ga potrebujemo ter da vsako glavo od matrike ki
		je oblike int list v funkcijo izpisi_stolpec2 z zeljenim stolpcom ter jo shrani v int list,
		ki je funkcija izpisi_stolpec z repom od matrike in zeljenim stolpcom. nato kot zgoraj pri
		tranponirai funkciji ko je glava matrike prazna klice rep in tako naprej dokler se matrika
		ne izprazne in pol se funkcija zaustavi **)
izpisi_stolpec moja_matrika2 2;;

(** 4. naloga **)
		
let rec mnoz_mat2 mat1 mat2= match mat2 with
| [] -> []
| []::rv -> mnoz_mat2 mat1 rv
| (gm::rm)::rv -> List.fold_left (+) 0 (List.map2 ( * ) (mat1) (gm::List.map List.hd rv))::mnoz_mat2 mat1 (rm::List.map List.tl rv);;
(** funckija mnoz_mat2 prejme matriko2 in glavo od matrike1 in z uporabo funkcije List.fold ki ji 
		je dan parameter (+) kar pomeni da elemente sesteva in zacne z 0. List.map2 daje List.fold_left
		elemente za sestevat in sicer prejme arugument ( * ) ki pomeni da bo mnozilo elemente 2 listou
		med sabo. prvi list je mat1 ki je head od mat1 podan s spodnjo funckijo, 2 list pa so glave
		od vseh elementov matrike2 in te elemente List.map2 mnozi prve iz usake liste, pol druge itd. in
		jih da v novo listo katere potem List.fold_left sesteje med sabo in vrne en sam int. ta int pol 
		rekurzivno ustavi v list ki je funkcija mnoz_mat2 s parametrom ki mat1, ki je se vedno glava od
		matrike1 in repom vseh elementov matrike2 da ponovi vajo in nakoncu vrne spodnji funkciji list
		z prvo vrstico zmnozene matrike nato spodnja funkcija dobljen list vstavi v list ki je spet 
		rekuzivno klicana funkcija mnoz_mat s prvim paramtrom ki je rep od matrike1 in drugi je matrika 2 
		in potem se funcija izvede in spet klice zgornjo funkcijo vendar tokrat z drugim elementom matrike1
		in tako vrne drugo vrstico dokler ne zmanka elementov v matriki 1 in se funkcija zakljuci. **)
let rec mnoz_mat mat1 mat2 = match mat1 with
| g::r when (List.length (List.hd mat1) != List.length mat2) -> raise (Failure "se ne da mnozit")
| [] -> []
| []::rv -> mnoz_mat rv mat2
| g::r -> mnoz_mat2 g mat2::mnoz_mat r mat2;;

mnoz_mat moja_matrika2 moja_matrika1;;
mnoz_mat moja_matrika1 moja_matrika2;;
mnoz_mat moja_matrika1 moja_matrika1;;

(** 5. naloga **)

let matrikaA = [[1;2;3];[4;5;6];[7;8;9]];;
let matrikaB = [[4;5];[7;8]];;

let rec subMat2 list1 list2 = match list1 with
| [] -> true
| g::r when List.hd list1 == List.hd list2 -> subMat2 (List.tl list1) (List.tl list2);;

let drzi a = 
	if a = 1 then true else false;;

let subMat1 mat1 mat2 = match mat2 with
	| g::r when (List.length mat1 < List.length mat2) ->raise (Failure "druga matrika je vecja od prve")
	| [] ->  []
	| g::r when List.length mat1 == 0 -> ["vsebje"]
	| []::vr -> subMat1 mat1 vr	
	| (mg::mr)::vr when subMat2 (List.hd mat1) (List.hd mr) -> subMat2 (List.tl mat1) (List.hd vr);;

(* 3. DN LANI *)

class virtual clovek ime priimek spol =
object
val ime = (ime:string)
val mutable priimek = (priimek:string)
val spol = (spol:char)
method virtual izpisiImePriimek :string
method virtual popraviPriimek : string -> unit
end;;

class moski ime priimek=
object
inherit clovek ime priimek 'm'
method izpisiImePriimek : string = (ime ^ " " ^ priimek)
method popraviPriimek novPriimek = priimek <- novPriimek
method izpisiPriimek = priimek
method izpisiIme = ime
end;;

class zenska ime priimek =
object
inherit clovek ime priimek 'z'
method izpisiImePriimek :string = (ime ^ " " ^ priimek)
method popraviPriimek novPriimek = priimek <- novPriimek
method izpisiPriimek = priimek
method izpisiIme = ime
end;;

class otrok ime spol=
object
inherit clovek ime "" spol
method izpisiImePriimek = (ime ^ " " ^ priimek)
method popraviPriimek novPriimek = priimek <- novPriimek
method izpisiPriimek = priimek
method izpisiIme = ime
end;;

let stupido = new moski "Stupido" "Bastardo";;
stupido#izpisiImePriimek;;

let mali = new otrok "mali" 'm';;
mali#izpisiImePriimek;;

let mala = new otrok "mala" 'z';;
mala#izpisiImePriimek;;

let stupida = new zenska "Stupida" "Bastarda";;
stupida#izpisiImePriimek;;

class druzina imeDruzine moz zena=
object
val moz = moz
val zena = zena
val mutable otroci = ([] :string list)
initializer zena#popraviPriimek (moz#izpisiPriimek)
method dodajOtroka (otrok:otrok) = 
otrok#popraviPriimek (moz#izpisiPriimek);
otroci <- (otrok#izpisiImePriimek):: otroci
method izpisi  =  (String.concat ", " [moz#izpisiImePriimek], String.concat ", " [zena#izpisiImePriimek]) , otroci
end;;

let familja = new druzina "Wosli" stupido stupida;;
familja#dodajOtroka mali;;
familja#dodajOtroka mala;;
familja#izpisi;;


(*SKLAD*)

(* Implementacija sklada *)


(* definicija tipa *)
type 'a stack = { mutable ind:int; size:int; mutable elts:'a array } ;;

(* inicializacija sklada *)
let init_stack n = {ind=0; size=n; elts =[||]} ;;

(****************************************)

let sk1 = init_stack 5;;

exception Stack_empty ;;
exception Stack_full ;;

push 8 sk1;;
push 3 sk1;;
push 6 sk1;;
pop sk1;;
top sk1;;




sk1;;

(****************************************)


(* funkcije za delo s skladom *)
exception Stack_empty ;;
exception Stack_full ;;
let pop p =
    if p.ind = 0 then raise Stack_empty
    else (p.ind <- p.ind - 1; p.elts.(p.ind)) ;;

let push e p =
    if p.elts = [||] then
    (p.elts <- Array.create p.size e;
    p.ind <- 1)
    else if p.ind >= p.size then raise Stack_full
    else (p.elts.(p.ind) <- e; p.ind <- p.ind + 1) ;;

let top p = 
	if p.ind = 0 then raise Stack_empty
	else p.elts.(p.ind-1);;



(* Uporaba *)
let p = init_stack 4 ;;

push 1 p ;;

for i = 2 to 5 do push i p done ;;

p ;;

pop p ;;

pop p ;;



(* Sklad z neomejenim st elementov*)
type 'a stack =
    {mutable ind:int ; mutable size:int ; mutable elts:'a array} ;;

(* popravimo push funkcijo *)
let push e p =
    if p.elts = [||]
    then
        begin
            p.elts <- Array.create p.size e;
            p.ind <- 1
        end
    else if p.ind >= p.size then
        begin
            let nt = 2 * p.size in
            let nv = Array.create nt e in
            for j=0 to p.size-1 do nv.(j) <- p.elts.(j) done ;
            p.elts <- nv;
            p.size <- nt;
            p.ind <- p.ind + 1
        end
    else
        begin
            p.elts.(p.ind) <- e ;
            p.ind <- p.ind + 1
        end ;;

(* Primeri *)
let p = init_stack 4 ;;

for i = 1 to 5 do push i p done ;;

p ;;





(*izpit*)
(* *)(* 2. *)
type 'a element = E of 'a | L of 'a list;;

let rec print sez = match sez with
	E x -> print_int x
	L s -> print_int (List.hd s); print (List.tl s);;
	

	
(* 3. *)
let calc sez =
	let x = ref (List.hd sez) in
	let rec racunaj s = match (List.tl s) with
		[] -> x := !x
		g1 :: g2 :: r when (g1 = Op PLUS) -> x := !x + g2; racunaj (g2 :: r)
		g1 :: g2 :: r when (g1 = Op TIMES) -> x := !x * g2; racunaj (g2 :: r)
	in
	racunaj sez; !x;;
	
	
(* 4. *)
class ['a] ArrayM (ini: 'a list) =
	object (self)
	inherit ['a] Array (ini: 'a) as super
	method get i = super#get i
	method set x i = x :: (self#get i)
	method del x i = List.filter (fun y -> y != x) (self#get i)
end;;