(*26.ta naloga iz zbirk vaj
26. Napiši funkcijo v Ocaml, ki združi dva seznama v tretji seznam tako, da vzame najprej en
element iz prvega seznama potem dva elementa iz drugega seznama in tako naprej dokler
ne pride do konca enega izmed vhodnih seznamov. Preostanek nepraznega seznama se da
na konec novega seznama.
Primer uporabe:
Seznam1 = [ 1 , 2 , 3 ]
Seznam2 = [ 5 , 6 , 7 , 8 ]
Rezultat : [1 ,5 ,6 ,2 ,7 ,8 ,3]
*)
let sez1 = [1;2;3]
let sez2 = [5;6;7;8](*6*)
let nt=([1;2;3],[1;5;6;2;7;8;3]);;
let sez3 = [];;

let rec skp nt = match nt with
| (x,[]) -> []
| (g::r,[]) -> g @ sez3 @ skp (r,[]);;

skp (sez1,sez2);;
	
let sez1 = [1;2;3]
let sez2 = [5;6;7;8](*6*)
(*kodo bi rabu*)

(*
27. Dan imamo seznam znakov tipa char list v katerem se lahko pojavijo samo znaka ’a’
in ’b’. Napiši funkcijo cnta :
char list -> int list, ki pretvori sekvence znakov v
seznam celih števil po naslednjih pravilih:
− aaa −> 3 ,
− aa −> 2 ,
− a −> 1 i n
− x −> 0 , k j e r j e x p o l j u b e n znak .
Primer:
# cnta [ ’ a ’ , ’ a ’ , ’ a ’ , ’ a ’ , ’ a ’ , ’ b ’ , ’ a ’ , ’ a ’ ] ; ;
− : int l i s t = [3 ,2 ,0 ,2]
treba delat od spredi.
*)


let rec cnta sez = match sez with
| [] -> []
| 'a'::'a'::'a'::r -> 3 :: cnta r (* aaa *)
| 'a'::'a'::r -> 2 :: cnta r (* aaa *)
| 'a'::r -> 2 :: cnta r (* aaa *)
| _::r -> 0 :: cnta r;;

cnta ['a';'a';'a';'a';'a';'a';'a';'a';'a';'a';'a';'a';'a'];;


(*∗ 5. Dano je drevo, ki vsebuje dve vrsti elementov. Definirano je z naslednjo podatkovno
strukturo:
type ( ’ a , ’ b ) drevo =
Prazno
| V o z l i s c e a o f ’ a ∗ ( ’ a , ’ b ) drevo l i s t ; ;
| V o z l i s c e b o f ’ b ∗ ( ’ a , ’ b ) drevo l i s t ; ;
Napiši funkcijo razcepi : (’a,’b) drevo -> ’a list * ’b list, ki prepiše vse elemente Vozliscea
v prvi seznam in vse elemente Vozlisceb v drugi seznam.*)
 

type ('a,'b) drevo =
	Prazno
	| Vozliscea of 'a * ('a,'b) drevo  (*nterica*)
	| Vozlisceb of 'b * ('a,'b) drevo;;  (*nterica*)

let dr1 = Vozliscea (9, Vozlisceb ("Je",Vozlisceb ("bo",Prazno)));;

(*rezultat bo  nterica ([9],[Je;bo])*)
let razcepi drevo=
	let a = ref [] in
	let b = ref [] in
let rec raz dr = match dr with
	| Prazno -> (!a,!b)
	| Vozliscea (x,y) -> a :=!a @ [x];raz y  (*zaradi vrstnega reda*)
	| Vozlisceb (x,y) -> b :=!b @ [x];raz y
	in
	raz drevo;; (*je klic funkcije, in notri da drevo ki je podan argument v funkcijo.*)

razcepi dr1;;

(**)
(* 7. Dan imamo seznam, definiran z rekurzivnim podatkovnim tipom izraz.
type izraz =
Nil
| Stevilo of int ∗ izraz
| Oper of char ∗ izraz;;

Izraz vsebuje aritmetične izraze, ki so lahko sestavljeni iz števil (Stevilo) in operacij (Oper).
Dovoljene operacije so plus ’+’ in minus ’-’. Predpostavljamo, da izrazi opisujejo pravilne
aritmetične izraze.
Napiši funkcijo ovrednoti : izraz -> int, ki izračuna vrednost izraza.
Primer:
e = 10 + 5 − 3
# let e = Stevilo (10, Oper ('+',Stevilo (5,Oper ('-',Stevilo(3,Nil)))));;
val e : izraz = Stevilo(10,Oper('+', Stevilo(5,Oper('-', Stevilo(3,Nil)))));;
# ovrednoti e ; ;
− : int = 12


*)
type izraz =
Nil
| Stevilo of int * izraz
| Oper of char * izraz;;

let e = Stevilo (10,Oper('+',Stevilo(5,Oper('-',Stevilo(3,Nil)))));;

(*dela, ni moje*)

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


let e = Stevilo(10, Oper('+', Stevilo(5,Oper('-', Stevilo(3,Nil)))));;
let f = Stevilo(0, Oper('+', Stevilo(5,Oper('-', Stevilo(0,Nil)))));;
ovrednoti e;;
ovrednoti f;;

(*               *)




















