

(*-------------------Funkcije in Funkcije višjega reda----------------------------*)
(*
1. Napiši funkcijo evroKalkulator, ki pretvarja iz evrov v tolarje in obratno. V primeru
neznane valute, javi napako.
Primer uporabe:
# evroKalkulator (50.0,"v sit");;
− : float∗string=(11982.,"eur v sit")
# evroKalkulator (100.0,"v eur");;
− : float∗string = (41.7292605575029256,"sit v eur")
# evroKalkulator (2.3,"v dolar");;
− : float ∗ string = (0.,"napaka")*)

let evroKalkulator (denar,pretvorba) = match pretvorba with
| "v sit" -> (denar *. 239.64,"evro v sit")
| "v eur" -> (denar /. 239.64,"sit v eur")
| _ -> (0.,"napaka");;

evroKalkulator (50.0,"v sit");;
evroKalkulator (50.0,"v eur");;
evroKalkulator (50.0,"v dolar");;

(*
2. Napiši funkcijo, ki izpiše večje število izmed dveh podanih. Primer uporabe:
# max (2,6);;
− : int = 6*)

let max (a,b) = 
	if a > b then a
	else b;;

max (2,25);;
max (5,22);;

(*
3. Napiši funkcijo, ki pove število elementov v seznamu. Primer uporabe:
# dolzina [1;2;3;4];;
− : int = 4*)

let dolzina sez = 
	let st = ref 0 in
	let rec recsz sez = match sez with
	| [] -> !st
	| g::r -> st := !st + 1; recsz r
	in
	recsz sez;;

dolzina [1;2;3;4];;

(*4. Napiši funkcijo, ki poišče dano število v seznamu. Kot parameter dobi število in seznam.
Primer uporabe:
# search 2 [1:2;3;4];;
− : bool = true
# search 3 [1;2;4;5];;
− : bool = false *)

let rec search stev sez = match sez with
| [] -> false
| g::r when g == stev -> true
| g::r -> search stev r;;

search 2 [1;2;3;4;5];;
search 12 [1;2;3;5];;

(*
5. Napiši funkcijo, ki izpiše n-to število Fibonaccijevega zaporedja. Fibonaccijevo zaporedje
je: 1, 1, 2, 3, 5, 8, ...
Primer uporabe:
# fib 1;;
− : int = 1
*)

let rec fibonacci n =
	if n < 2 then 1
		else
			fibonacci (n-1) + fibonacci (n-2);;

fibonacci 2;;

(*6. Napiši funkcijo, ki izpiše seznam do n-tega Fibonaccijevega števila za nazaj (pomagaj si z
zgornjo funkcijo).
Primer uporabe:
# fibSez 9;;
− : int list = [34;21;13;8;5;3;2;1;1]*)
let fibSez n = 
  let rec fibonacci n =
  	if n < 2 then 1
  		else
  	fibonacci (n-1) + fibonacci (n-2)
  in
  let rec fibez x =
  	if x < 0 then []
  		else
  	[fibonacci x] @ fibez (x-1) 
  in 
	fibez (n-1);;
  					
fibSez 9;;

(*7. Napiši funkcijo, ki obrne dani seznam.
Primer uporabe:
# obrni [1;2;3;4];;
− : int list= [4;3;2;1]*)

let rec obrni sez = match sez with
| [] -> []
| g::r -> obrni r @ [g];;

obrni [1;2;3;4;123123123123;44444];;


(*8. Napiši funkcijo, ki izpiše seznam do n-tega Fibonaccijevega števila za naprej (pomagaj si
z zgornjimi funkcijami).
Primer uporabe:
# fibSez2 9 ; ;
− : int list = [1;1;2;3;5;8;13;21;34]*)
let fibSez3 n = 
	let rec fibonacci n =
	if n < 3 then 1
		else
			fibonacci (n-1) + fibonacci (n-2)
in
let rec fibSez3 n = match n with 
| 0 -> []
| _ -> fibSez3 (n-1) @ [fibonacci n]
in
fibSez3 n;;

fibSez3 9;;

(*9. Definiraj funkcijo, ki sešteje števila do n.
Primer uporabe:
# sestejDo 4 ; ;
− : int = 10*)

let sestejDo n =
	let st = ref 0 
	in
	for i=0 to n do 
		st := !st + i
	done;
	!st;;

sestejDo 3;;

(*10. Definiraj funkcijo, ki izračuna fakulteto števila n.
Primer uporabe:
# fakulteta 4;;
− : i n t = 24*)

let rec fakulteta x =
	if x = 0 then 1
		else 
	x * fakulteta (x-1);;

fakulteta 5;;

(*
11. Definiraj funkcijo, ki generira seznam števil do števila n.
Primer uporabe:PROGRAMIRANJE II
5
# seznamDo 9 ; ;
− : int list = [1;2;3;4;5;6;7;8;9] *)

let rec seznamDo n = 
	if n = 0 then []
		else
	(seznamDo (n-1)) @ [n];;

seznamDo 99;;


(* 12. Definiraj funkcijo, ki sešteje vsa števila v seznamu.
Primer uporabe:
# seznamSes [1;4;6];;
− : int = 11 *)


let rec seznamSes sez = match sez with
| [] -> 0
| g::r -> g + seznamSes r;;

seznamSes [1;4;6] ;;


(*13. Napiši funkcijo, ki izračuna obseg trikotnika. Kot parameter dobi stranice v obliki n-teric.
Primer uporabe:
# obseg (4,2,8);;
− : int = 14*)

let obseg (a,b,c) =
	a+b+c;;
obseg (4,2,8);;

(*14. Napiši funkcijo, ki izračuna ploščno pravokotnega trikotnika (recimo a in b sta pravokotna).
Kot parameter dobi stranice v obliki n-teric.
Primer uporabe:
# ploscina (2.5 ,4.7 ,5.1);;
− : float = 5.875*)

let ploscina (a,b,c) =
	(a*.b) /. 2.0;;

ploscina (2.5,4.7,5.1);;


(*15. Napiši funkcijo v OCaml, ki dobi seznam in število, vrne pa seznam, ki vsebuje samo
elemente večje od podanega števila.
Primer uporabe:
− vecjeOd ([2;5;26;87;2;6],5);;
val i t = [26;87;6] : int list*)

let rec vecjeOd (sez,n) = match sez with
| [] -> []
| g::r -> if g > n then [g] @ vecjeOd (r,n) else vecjeOd (r,n);;

vecjeOd ([2;5;26;87;2;6],5);;

(*16. Definiraj funkcijo, ki preveri če so vsi elementi seznama pozitivni*)

let rec vecSez sez = match sez with
| [] -> true
| g::r -> if g > 0 then vecSez r else false;; 

vecSez [1;2;4;5];;
vecSez [0;2;3];;
vecSez [-2;2;0];;

(*17. Definiraj funkcijo, ki kvadrira elemente seznama*)

let rec kvadrSez sez = match sez with
| [] -> []
| g::r -> [g*g] @ kvadrSez r;;

kvadrSez [1;2;3;0;4];;

(*18. Definiraj funkcijo, ki pomnoži elemente seznama z x*)

let rec kvadrMnoz sez x = match sez with
| [] -> []
| g::r -> [g*x] @ kvadrMnoz r x;;

kvadrMnoz [1;2;3;0;4] 2;;


(*19. Definiraj funkcijo, ki dobi seznam:int list, vrne pa seznam:bool list, kjer je true v primeru,
da je število sodo in false v primeru, da je število liho.*)

let rec trueSez sez = match sez with
| [] -> []
| g::r when g mod 2 = 0 -> [true] @ trueSez r
| g::r -> [false] @ trueSez r;;

trueSez [1;2;3;0;4];;

(* 20. Z uporabo ukaza filter definiraj funkcijo, ki iz seznama odstrani števila večja od x.*)

let filterSez sez x = List.filter (fun y -> x>y) sez;;

filterSez [1;2;3;0;4] 2;;


(*21. Definiraj funkcijo, ki podani besedi zamenja velikost črk (npr. za "BanAna"izpiše "bA-
NaNA", za "BaNaNa"izpiše "bAnAnA", ipd.). Pomoč: velike črke se v ASCII tabeli naha-
jajo na položajih med 65 (A) in 90 (Z), male pa med 97 (a) in 122 (z).
Primer uporabe:
− velikeMale "BaNana";;
val i t = "bAnANA" :string*)

let velikeMale str = 
	let res = ref ""
	in
	for i=0 to (String.length str)-1 do
		if (int_of_char str.[i]) < 91 then
			res := !res ^ String.make 1 (char_of_int ((int_of_char str.[i]) + 32)) 
		else
			 if int_of_char (str.[i]) > 96 then
			res := !res ^ String.make 1 (char_of_int ((int_of_char str.[i]) - 32)) 
		done;
		!res;;

velikeMale "BaNana";;


(*22. Definiraj funkcijo, ki izpiše abecedo.*)

let abeceda = 
	for i=0 to 25 do
	print_char (char_of_int (i+65))
	done;;



(*23. Definiraj funkcijo, ki pomakne črke za en položaj (po želji naj ’z’ zamenja za ’a’).
Primer uporabe:
# povecajNiz "abc" ;;
− : string = "bcd"
# povecajNiz "banana" ;;
− : string = "cbobob"*)

let povecajNiz niz =
	String.map 
	(fun x -> (if (int_of_char x) <= 121 (*ce je stevilka manjsa *)
	 then 
		char_of_int((int_of_char x) + 1) 
	else 
		(char_of_int((int_of_char x)-25 )) ) ) niz ;;

povecajNiz "bananaz";;

(*24. Definiraj map funkcijo (funkcijo, ki se sprehodi po elementih seznama in nad nijimi izvede
podano operacijo).*)

let listMap sez func = 
let rec sezmap plist fnc = match plist with
| [] -> []
| g::r -> [fnc(g)] @ sezmap r fnc
in 
sezmap sez func;;
 
listMap [1;2;3] (fun x -> x*x) ;; 

(*∗∗ 25. Definiraj funkcijo v Ocaml-ju, ki za dani seznam posebej sešteje soda in liha števila, ter
vrne 2-terico, ki ima na prvem položaju vsoto lihih števil, na drugem pa vsoto sodih števil.
Primer uporabe:
− vsotaSodeLihe  ([1;1;1;2;4]);;
val i t = (3 ,6) : int ∗ int*)

let vsotaSodeLihe (sez) =
	let soda = ref 0 and liha = ref 0 
	in 
let rec vsotasl (sez) = match sez with
| [] -> (!liha,!soda)
| g::r when g mod 2 = 0 -> soda := !soda + g; vsotasl (r)
| g::r -> liha := !liha + g;vsotasl (r)
in vsotasl (sez);;

vsotaSodeLihe ([1;1;1;2;4]);;

(*26. Napiši funkcijo v Ocaml, ki združi dva seznama v tretji seznam tako, da vzame najprej en
element iz prvega seznama potem dva elementa iz drugega seznama in tako naprej dokler
ne pride do konca enega izmed vhodnih seznamov. Preostanek nepraznega seznama se da
na konec novega seznama.
Primer uporabe:
Seznam1 = [1;2;3]
Seznam2 = [5;6;7;8]
Rezultat : [1;5;6;2;7;8;3]*)

let zdruziSez sez1 sez2 = 
	let s = ref [] in
	let rec zdsez (a,b) = match (a,b) with
	| ([],[]) -> []
	| ([],g::[]) -> s := !s @ [g];!s
	| (g::[],[]) -> s := !s @ [g];!s 
	| (g::r,g2::g3::r2) -> s:= !s @ ([g] @ [g2] @ [g3]); zdsez (r,r2)
	in
	zdsez (sez1,sez2);;

zdruziSez [1;2;3] [5;6;7;8];;

(*kramar*)
let zdruziSez sez1 sez2 = 
		let rec zdsez (a,b) = match (a,b) with
	| ([],x) -> x
	| (x,[]) -> x
	| (g::[],g2::r2) -> g::g2::r2
	| (g2::r2,g::[]) -> g::g2::r2
	| (g::r,g2::g3::r2) -> [g] @ [g2] @ [g3] @ zdsez (r,r2)
	in
	zdsez (sez1,sez2);;

zdruziSez [1;2;3] [5;6;7;8];;



(*27. Napiši funkcijo "izberi(s,l): ’a list * int -> ’a list", ki iz seznama s izbere samo tiste
elemente, ki so večji od l in jih vrne kot rezultat funkcije.*)

let rec izberi (sez,n) = match sez with
| [] -> []
| g::r -> if g > n then [g] @ izberi (r,n) else izberi (r,n);;

izberi ([1;2;3;4;5;6;7],4);;

(*28. Napišite funkcijo, ki izpiše seznam števil od števila n do m. Velja da n<m.
primer uporabe:
− seznamNM (5,11);;
val i t = [5;6;7;8;9;10;11] : int list*)

let rec seznamNM (n,m) = match n with
| n when n=m -> [] @ [m]
| n -> [n] @ seznamNM ((n+1),m);;

seznamNM (5,11);;


(*29. Naredi funkcijo odstrani(int list, int) v Ocaml, ki iz seznama odstrani vsa števila,
ki so manjša od podanega števila n. Preostanek števil seznama naj pomnoži z n.
Primer uporabe:
# odstrani ([1;2;3;4;5;6;7;8],5);;
− : int list = [25;30;35;40]*)

let rec odstrani (sez,n) = match sez with
| [] -> []
| g::r when g < n -> odstrani (r,n)
| g::r when g > n -> [g*n] @ odstrani (r,n)
| g::r when g = n -> [g*n] @ odstrani (r,n);;

odstrani ([1;2;3;4;5;6;7;8],5);;
odstrani ([1;2;3;4;5;6;7;8],5);;


(*30. Dana je funkcija seštej: a’ list -> int, ki sešteje elemente danega seznama.
Primer uporabe:
# sestej [1;2;3];;
− : int = 6*)

let sestej sez = 
	let st = ref 0 in
	let rec ses sez = match sez with
	| [] -> !st
	| g::r -> st := !st + g; ses r
	in
	ses sez;;  
(*Predstavi vsa stanja aktivacijskih zapisov ob klicu funkcije sestej [1;2;3].????????*)
#trace sestej;;

sestej [1;2;3];;
		
(*31. Dana je OCaml funkcija naslednik (let naslednik n = n+1). Z uporabo funkcije naslednik in
brez uporabe artimetičnih operacij definiraj funkcijo jeVsota(a:int, b:int, c:int), ki preveri
ali je c vsota a in b (a in b sta >= 0).*)



(*
32. Dan je seznam znakov tipa char list. Napiši funkcijo palindrom: char list -> bool, ki
preveri ali je dan seznam palindrom.
Primer uporabe:
# palindrom [’p’;’e’;’r’;’i’;’c’;’a’;’r’;’e’;’z’;’e’;’r’;’a’;’c’;’i’;’r;’e’;’p’];;
−: bool = true
*)
(*dve rešitvi*)
let palindrom sez =
	let rec pal sez = match sez with
	| [] -> []
	| g::r -> pal r @ [g]
	in
	if sez = (pal sez) then true else false;;

palindrom ['p';'e';'r';'i';'c';'a';'r';'e';'z';'e';'r';'a';'c';'i';'r';'e';'p'];;

let ispalindrom sez = sez = List.rev sez;;
ispalindrom ['p';'e';'r';'i';'c';'a';'r';'e';'z';'e';'r';'a';'c';'i';'r';'e';'p'];;

(*∗ 33. Napiši funkcijo zamenjaj :
int list -> int list, ki v enem prehodu poišče v se-
znamu celih števil vse zaporedne pare x::y, ki niso urejeni po naraščajočem vrstnem redu
(x<=y) in jih obrne.
Z uporabo funkcije zamenjaj implementiraj sortiranje seznama.*)

let rec zamenjaj sez = match sez with
| [] -> []
| [x] -> [] @ [x]
| g::g2::r when g > g2 -> (zamenjaj (g::r)) @ [g2]
| g::g2::r when g < g2 -> (zamenjaj (g2::r)) @ [g]
| g::g2::r when g = g2 -> zamenjaj (g2::r);;

#trace zamenjaj ;;
zamenjaj [2;9;8;7;6;5;4;3;2;1];;

(*∗∗ 34. Dan je seznam parov r, ki definira relacijo R med števili. Napiši funkcijo
razsiri :
int*int list -> int list -> int list,
kjer je prvi parameter seznam parov r in drugi parameter seznam celih števil s. Rezultat
funkcije razsiri naj bo seznam, ki vsebuje vsa števila x za katera velja (e,x)∈R za nek
element e seznama s.
*)



(*
∗ ∗ ∗ 35. Dano imamo sekvenco DNK v obliki seznama znakov.
Na primer, niz znakov "ACAAGATGCCATTGTCCCCCGACAACCCCAGCCACAC" spremenjen v se-
znam znakov.
Napišite funkcijo isci :
char list -> unit, ki poišče najdaljši podniz in ga izpiše.*)

