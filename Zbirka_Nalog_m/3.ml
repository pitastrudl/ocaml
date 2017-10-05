
(*3
3. VZORCI IN PRENOS PARAMETROV
Vzorci in prenos parametrov ----------------------------------*)

(*∗ 1. Z uporabo uemanja vzorcev rešite vse naloge prejšnjega poglavja (Funkcije). PA SI TI NOR HAHAHAH*)

(*2. Definiraj funkcijo, ki določi kakšen je dani znak. Možnosti naj bodo: samoglasnik, sogla-
snik, številka, drugo*)

let znak n = match n with
|'a' -> "samoglasnik"
|'e' -> "samoglasnik"
|'i' -> "samoglasnik"
|'o' -> "samoglasnik"
|'u' -> "samoglasnik"
|'b' -> "soglasnik"
|'c' -> "soglasnik"
|'d' -> "soglasnik"
|'f' -> "soglasnik"
|'g' -> "soglasnik"
|'h' -> "soglasnik"
|'j' -> "soglasnik"
|'k' -> "soglasnik"
|'l' -> "soglasnik"
|'m' -> "soglasnik"
|'n' -> "soglasnik"
|'p' -> "soglasnik"
|'r' -> "soglasnik"
|'s' -> "soglasnik"
|'t' -> "soglasnik"
|'v' -> "soglasnik"
|'z' -> "soglasnik"
|'0'..'9' -> "stevilo"
| _  -> "drugo";;

znak '.';;
znak '9';;
znak 'a';;

(*3. Z uporabo ujemanja vzorcev napiši funkcijo evroKalkulator, ki pretvarja iz evrov v tolarje
in obratno. V primeru neznane valute, javi napako.
Primer uporabe:
# evroKalkulator (50.0 , "v s i t " ) ; ;
− : f l o a t ∗ s t r i n g = ( 1 1 9 8 2 . , " eur v s i t " )
# evroKalkulator ( 100 00. 0 , " v eur " ) ; ;
− : f l o a t ∗ s t r i n g = (41.7292605575029256 , " s i t v eur " )
# evroKalkulator (2.3 , "v dolar " ) ; ;
− : f l o a t ∗ s t r i n g = ( 0 . , " napaka " )*)


let evroKalkulator (denar,pretvorba) = match pretvorba with
| "v sit" -> (denar *. 239.64,"evro v sit")
| "v eur" -> (denar /. 239.64,"sit v eur")
| _ -> (0.,"napaka");;

evroKalkulator (50.0,"v sit");;
evroKalkulator (100.0,"v eur");;
evroKalkulator (2.3,"v dolar");;


(*4. Z uporabo ujemanja vzorcev napiši funkcijo, ki izpiše n-to število Fibonaccijevega zapo-
redja. Fibonaccijevo zaporedje je: 0, 1, 1, 2, 3, 5, ...
Primer uporabe:
# fib 1;;
− : int = 0
# fib 2;;
− : int = 1
# fib 3;;
− : int = 1
# fib 10;;
− : i n t = 34*)

(*4. Z uporabo ujemanja vzorcev napiši funkcijo, ki izpiše n-to število Fibonaccijevega zapo-
redja. Fibonaccijevo zaporedje je: 0, 1, 1, 2, 3, 5, ...
Primer uporabe:
# fib 1;;
− : int = 0
# fib 2;;
− : int = 1
# fib 3;;
− : int = 1
# fib 10;;
− : int= 34
*)
let rec fib n = match n with
|0 -> 1
|1 -> 1
|2 -> 1
| _ -> fib (n-1) + fib (n-2);;

fib 10;;


(*
5. Z uporabo ujemanja vzorcev napiši funkcijo, ki izpiše seznam do n-tega Fibonaccijevega
števila za nazaj (pomagaj si z zgornjo funkcijo).
Primer uporabe:
# fibSez 10;;
− : int l i s t = [ 3 4 ; 21; 13; 8; 5; 3; 2; 1; 1; 0]
# fibSez 4;;
− : int l i s t = [ 2 ; 1; 1; 0]
*)
let fibSez n = 
let rec fib n = match n with
|0 -> 1
|1 -> 1
|2 -> 1
| _ -> fib (n-1) + fib (n-2)
in
let rec fibs n = match n with
| 0 -> []
| _ -> [fib n] @ fibs (n-1)  
in fibs n;;

fibSez 10;;


(*
6. Napiši funkcijo, ki obrne dani seznam.
Primer uporabe:
# obrni [1;2;3;4];; 
− : int list = [4;3;2;1]
*)

let rec obrni sez = match sez with
| [] -> []
| g::r -> (obrni r) @ [g];;
obrni [1;2;3;4];; 

(*
7. Z uporabo ujemanja vzorcev napiši funkcijo, ki izpiše seznam 
do n-tega Fibonaccijevega števila za naprej 
(pomagaj si z zgornjimi funkcijami).
Primer uporabe:
# fibSez2 9;;
− : int list = [1;1;2;3;5;8;13;21;34]
# fibSez2 14;;
− : int list = [1;1;2;3;5;8;13;21;34;55;89;144;233;377] *)

let fibSez n = 
let rec fib n = match n with
|0 -> 1
|1 -> 1
|2 -> 1
| _ -> fib (n-1) + fib (n-2)
in
let rec fibs n = match n with
| 0 -> []
| _ ->  fibs (n-1) @ [fib n]  
in fibs n;;

fibSez 10;;



(*
8. Z uporabo ujemanja vzorcev napiši funkcijo, ki izpiše večje število izmed dveh podanih.
Primer uporabe:
# max (2,6);;
− : int = 6*)
let max (a,b) = match a with
| _ -> if a > b then a else b;;
max (2,6);;

(*
9. Z uporabo ujemanja vzorcev napiši funkcijo, ki pove število elementov v seznamu.
Primer uporabe:
# dolzina [1;2;3;4];;
− : int = 4
*)
let dolzina sez =
	let rec dol sez i = match sez with
	| [] -> i
	| g::r -> dol r (i+1)
	in
	dol sez 0;;

dolzina [1;2;3;4;5];;


(*
10. Z uporabo ujemanja vzorcev napiši funkcijo, ki poišče dano število v seznamu. Kot para-
meter dobi število in seznamt.
Primer uporabe:
# search 2 [1;2;3;4];;
− : bool = true
# search 3 [1;2;4;5];;
− : bool = false
*)
let rec search n sez = match sez with
| [] -> false
| g::r -> if g=n then true else search n r;;

search 2 [1;2;3;4];;
search 22 [1;2;3;4];;

(**)
(* 
11. Napiši funkcijo, ki niz:string prepiše v seznam:char list. *)

let explode str = 
	let rec exp str b = match b with
	| -1 -> []
	| b -> (exp str (b-1)) @ [str.[b]]
	in exp str ((String.length str)-1);;
	
explode "banana";;


(*
12. Napiši funkcijo, ki seznam:char list združi v niz:string.
*)

let zdruzi ses = 
	let rec zdru sez = match sez with
	| [] -> ""
	| g::r -> (String.make 1 g) ^ (zdru r)
	in 
	zdru ses;;

zdruzi ['b'; 'a'; 'n'; 'a'; 'n'; 'a'];;

(*
13. Napiši funkcijo, ki razpolovi seznam tako, da sešteje po dva in dva elementa. Če ima liho
mnogo elemenotov, zadnjega prepiše.
*)




(*
14. Napiši funkcijo, ki sešteje elemente seznama z uporabo ujemanja vzorcev.*)


let rec sessez sez = match sez with
	| [] -> 0
	| g::r -> g + sessez r;;

sessez [1;2;3;4];;

(*
15. Napiši funkcijo, ki dobi seznam in število, vrne pa seznam, ki vsebuje samo elemente večje
od podanega števila.
*)
let rec vecjeOd (sez,n) = match sez with
| [] -> []
| g::r -> if g > n then [g] @ vecjeOd (r,n) else vecjeOd (r,n);;

vecjeOd ([2;5;26;87;2;6],5);;



(*
16. Napiši funkcijo, ki vrne n-ti element iz seznama. 
Kot parameter dobi n in seznam. Če je
n večji od dolžine seznama, naj vrne 0.
*)



(*
17. Napiši funkcijo, ki preveri ali sta podana seznama enako dolga.
 Uporabi ujemanje vzorcev.
*)



(*
18. Napiši funkcijo, ki preveri ali je podani seznam palindrom.
*)



(*
19. Napiši funkcijo, ki preveri ali je podani niz palindrom.
*)



(*
∗∗ 20. Na predavanjih smo si ogledali abstraktno podatkovno strukturo Sklad.
Oglejmo si naslednje vrstice programske kode:
c l a s s Sklad {
v o i d push ( i n t e l t ) {
// koda
};
v o i d pop ( ) {
// koda
};
i n t top ( ) {
// koda
};
}
Preverjanje pravilnosti gnezdenih oklepajev predstavlja prvi korak pri izračunavanju enačb.
Napišite program, ki s pomočjo priloženega sklada ugotovi ali predstavlja vnešeni niz
oklepajev pravilno gnezdeno zaporedje:
( ) OK
( ) ( ) OK
( ( ) ) ( )OK
( ) ) ( NEOK
( ) ( ) ( ( ) ) )NEOK
*)



(*
∗∗ 21. Branje datotek ter sestavljanje nizov.
Napišite program, ki prešteje število ponovitev besede žajček"v datoteki. Privzemimo, da
so besede v datoteki ločene s presledki.
Namig: program naj prebere vsebino datoteke v polje nizov. Za vsak niz preštejemo število
pojavitev iskane besede.
Posebno področje vede o razpoznavanju vzorcev (Patern recognition) je iskanje vzorcev
na nizih (string pattern recognition).
Pogosto opravilo takšnih sistemov je iskanje pravil v vseh podnizih določenega vhodnega
niza.
Vaša naloga je izdelati metodo, ki za vhodni niz vrne polje, ki vsebuje vse podnize tega
niza.
Podpis metode:
S t r i n g [ ] p o d n i z i ( S t r i n g vhod ) ;
Oglejmo si še primer: ("miza", "miz", "mi", "m", "iza", "iz", "i", ža", ž", "a")
Iz primera lahko razberete enega od možnih algoritmov.
*)



(*
∗ ∗ ∗ 22. Računalniška črno-bela slika je predstavljena z razredom Slika. Osvetljenost točk je pred-
stavljena z vrednostmi od 0-254.
class Slika {
int [100][100] slika ;
Slika ( ) ;
}
c l a s s Vzorec {
int [ 10 ] [1 0 ] slika ;
Vzorec ( ) ;
}
Napiši metodo, ki poišče pojavitev vzorca (primerka Vzorca) v sliki (primerku Slika). Sami
definirajte vmesnik metode.
Podobnost med dvema vzorcema je definirana na osnovi podobnosti posameznih pik. Če
se dve pike razlikujeta manj kot je vrednost spremenljivke static int Toleranca; potem so
pike enake. Kako bi razširili metodo tako, da bi iskala podobne vzorce?
*)



(*
∗∗ 23. Napiši funkcijo v Ocaml, ki za dan vhodni niz znakov vrne "pravilnošamo v primeru, da
niz vsebuje vzorec "a+b+"t.j. enemu ali več znakov "ašledi eden ali več znakov "b".
Primer pravilnega niza:
" uzgaaabbbbdvcg "12
3. VZORCI IN PRENOS PARAMETROV
*)



(*
24. Oglejmo si funkcijo String beri(), ki prebere vsebino iz datoteke.
Funkcija proži izjemo IOException.
Napišite programsko kodo, ki z uporabo predstavljene funkcije beri() prebere vsebino da-
toteke, bodite pozorni na izjeme!
*)



(*
∗∗ 25. Napiši funkcijo v Ocaml, ki preveri ali je nek niz podniz nekega drugega niza. Pri tem ni
nujno, da znaki drugega niza v prvem stoje zaporedoma, ujemati se mora le vrstni red.
Primeri:
MATI MATEMATIKA SENO
MAT
MA
I
S
TI
M
SOSEDNOST
E NO
SE NO
ATI
MATI
Dodatna naloga: napiši funkcijo, ki vrne število vseh takšnih podnizov v danem nizu.
*)



(*
∗ 26. Funkcija pfib: int*int -> int*int je definirana na sledeč način:
pfib ( i , j ) = |
(1 ,1);
č e i <=0 i n j <=0,
| p f i b ( i −1 ,0) + ( 1 , 0 ) ; č e j =0
| p f i b ( 0 , j −1) + ( 0 , 1 ) ; č e i =0
| p f i b ( i −1, j −1) + p f i b ( i −2, j −2);
za v s e druge i i n j .
Operacija ’+’ je definirana nad pari na običajen način. Definiraj funkcijo pfib v Ocaml.
*)



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
− : int l i s t = [3 ,2 ,0 ,2]*)
