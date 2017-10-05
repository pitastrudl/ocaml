
(*alodostjeze.ml*)
(*Hello It's me :D*)


(***************************************************)
(***************************************************)
(***************************************************)
							(*Osnovne strukture*)
(***************************************************)
(***************************************************)
(***************************************************)

(*Osnovne strukture - 1*)
let a = 0.0;;
let a = false;;
let a = "Kejjssi";;
let a = (false, true);;
let a = (0.0, 99, 'a');; (*float int char*)
let list = (['a','c','a','b']);;
let a = ((1,2),(4,5));;
let a = (1,(true,('a',()), 3.1459658),23,"jojojo");;
let a = ("banana hmm", ("ssss",23),[2], false);;
(*Osnovne strukture - 2*)
not ( 1 = 2 ) && 1.0 < 2.0;; (*not(false) AND true ====> true And true ======> true*)
(*Osnovne strukture - 3*)
let a = 9;;
let b = 12;;
let c = (float_of_int a) +. (float_of_int b);;
print_float c;;
(*Osnovne strukture - 4*)
(*glej tretjo*)
(*Osnovne strukture - 5*)
let a = 9;;
let b = 12;;
let c = 15;;
a*b*c;;
(*Osnovne strukture - 6*)
let ime = "Ime";;
let telefon = 213;;
let sez = (ime, telefon);;
(*Osnovne strukture - 7*)
let a = 9;;
let b = 12;;
if (a < b) then a else b;;
(*Osnovne strukture - 8*)
let malaCrka = 'a';;
let velikaCrka = char_of_int(int_of_char malaCrka - 32)
(*Osnovne strukture - 9*) (*n-terice*)
let a = (44,22);;
let b = (fst a,snd a);;
(*Osnovne strukture - 10*)
let a = [1;2;3;4;5];;
let b = [6;7;8;9;10];;
let c = List.append a b
List.hd c
List.tl c
(*Osnovne strukture - 11*)
let c = [2] @ c @ [15];; (*zakaj @?*)

(*Osnovne strukture - 12*)
let a = [1; 2; 4; 7; 2];;
let b = [2; 4; 7; 9];;
vrnuUnijo a b
let vrnuUnijo sez1 sez2=
	let final = ref [] in
	let rec jeElement head sez = match sez with
	| [] -> false
	| g::r when (head=g) -> true
	| g::r -> jeElement head r
	in
	let rec neki sez1 = match sez1 with
	| [] -> []
	| g::e when (jeElement g !final) -> neki e
	| g::e -> final := !final @ [g]; neki e
	in
	neki sez1; neki sez2;	!final;;

(*Osnovne strukture - 13*)

let niz = "banana";;
sprehod niz;;
let sprehod niz=
	for l = 0 to (String.length niz - 1)do		
  	for i=l to (String.length niz - 1)do
  		print_char ',';
  		for j=i to (String.length niz - 1)do
  				print_char niz.[j];
  		done
  	done
	done

(***************************************************)
(***************************************************)
(***************************************************)
										(*Funkcije*)
(***************************************************)
(***************************************************)
(***************************************************)


(*Funkcije 1*)
let evroKalkulator money operator= match operator with
| x when x = "v sit" -> (money *. 11982. /. 50.,"eur v sit")
| x when x = "v eur" -> (money *.  50. /. 11982., "sit v eur")
| x -> failwith "OMG!!!!" (*ne da se mi pisat nterice*)
(*Funkcije 2*)
let max (a,b) = if (a<b) then b else a;;
(*Funkcije 3*)
let dolzina sez = List.length sez;;
(*Funkcije 4*)
let rec search ind sez = match sez with
| g::r when (g=ind) -> true
| g::r -> search ind r
| [] -> false;;
(*Funkcije 5*)
let rec fib mesto = match mesto with
| 1 -> 1
| 2 -> 1
| x -> (fib (x-1)) + (fib (x-2));;
(*Funkcije 6*)
let rec fib1 mesto = match mesto with
| 1 -> [1]
| 2 -> [1;1]
| x -> [fib (x)] @ fib1 (x-1);;
(*Funkcije 7*)
let rec obrni sez = match sez with
| [] -> []
| g::r -> obrni r@[g];;
(*Funkcije 8*)
let rec fib2 mesto = obrni (fib1 mesto)
(*Funkcije 9*)
let rec sestejDo stevilo= match stevilo with
| 0 -> 0
| x -> x+ sestejDo (x-1);;
(*Funkcije 10*)
let rec fakulteta n = match n with
| 0 -> failwith "napacno stevilo? nwm kaj je famnit"
| 1 -> 1
| x -> x*fakulteta (x-1);;

(*Funkcije 11*)
let seznamDo n = 
	let rec neki n i = match i with
	| x when(x=n) -> [x]
	| x -> [x]@ neki n (x+1)
	in 
	neki n 1;;
(*Funkcije 12*)
let rec sestejSez sez = match sez with
| [] -> 0
| g::r -> g+sestejSez r;;
(*Funkcije 13*)
let obseg n = match n with
| (a, b, c) -> a+b+c
(*Funkcije 14*)
let ploscina n = match n with
| (a, b, c) when (c>a && c>b) -> a *. b /. 2.
| (a, b, c)	when (b>a && b>c)	-> a *. c /. 2.
| (a, b, c) -> b *. c /. 2.		
(*Funkcije 15*)
let rec vecji n sez = match sez with
| [] -> []
| g::r when (g>n) -> [g] @ vecji n r
| g::r -> vecji n r
(*Funkcije 16*)
let rec pozitvno sez = match sez with
| [] -> true
| g::r when(g<0) -> false
| g::r -> pozitvno r
(*Funkcije 17*)
let rec kvadriraj sez = match sez with
| [] -> []
| g::r -> [g*g]@ kvadriraj r
(*Funkcije 18*)
let rec pomnozi sez x = match sez with
| [] -> []
| g::r -> [g*x]@ pomnozi r
(*Funkcije 19*)
let rec sodost sez = match sez with
| [] -> []
| g::r when (g mod 2=0) -> [true] @ sodost r
| g::r -> [false] @ sodost r
(*Funkcije 20*)
let neki sez x= List.filter (fun s -> s<x) sez
(*Funkcije 21*)
let neki niz =
	for i=0 to String.length niz-1 do
		if(int_of_char niz.[i] < 90) then print_char(char_of_int (int_of_char niz.[i] + 32))
		else print_char(char_of_int (int_of_char niz.[i] - 32))
			
	done ;;
(*Funkcije 22*)
(*ne da se mi*)	
(*Funkcije 23*)
(*ni fora naloge ce lahko vse crke zamnenjas.. sploh nerabis pol premaknit.. direkt zamnjas*)
(*Funkcije 24*)
let rec neki f sez= match sez with
| [] -> []
| g::r -> [f g] @ neki f r
(*Funkcije 25*)
let sestej sez = 
	let soda = ref 0 in
	let liha = ref 0 in
	let rec sprehodi s = match s with
	| [] -> (!soda,!liha)
	| g::r when (g mod 2 = 0) -> soda:= !soda+1; sprehodi r
	| g::r -> liha:= !liha+1; sprehodi r
	in
	sprehodi sez;
(*Funkcije 26*)
(*smo nardili na faksu*)
(*Funkcije 27*)
let neki x =  
	let rec neki1 sez l = match sez with
	| [] -> []
	| g::r when (g>l) -> [g] @ neki1 r l
	| g::r -> neki1 r l
	in 
	match x with
	| (s, l) -> neki1 s l;;
(*Funkcije 28*)
let seznamNM n m= 
	let rec neki n m i = match i with
	| x when(x=m) -> [x]
	| x -> [x]@ neki n m (x+1)
	in 
	neki n m n;;
(*Funkcije 29*)
let neki x =  
	let rec neki1 sez l = match sez with
	| [] -> []
	| g::r when (g>l) -> [g*l] @ neki1 r l
	| g::r -> neki1 r l
	in 
	match x with
	| (s, l) -> neki1 s l;;	
	
(*Funkcije 30*)	
(*tle napises algoritem ki to nardi -> najboljsji algoritem je koda in jo potem kometniraj z vsemi vmesnimi koraki*)
	
(*Funkcije 31*)
(*kar neki... naredi s forom in mas loop operacij z nasldenikom*)
(*Funkcije 32*)	
let palindrom sez=
	let reverse = List.rev sez in
	let rec deli (sez1,sez2) = match (sez1,sez2) with
	| (g1::r1, g2::r2) when (g1=g2) -> deli (r1,r2)
	| ([],[]) -> true
	| (g1::r1, g2::r2) when (g1!=g2) -> false
	in
	deli (sez,reverse);;
(*Funkcije 33*)	
zamenjaj [3;1;2;1;4];;
let sez = [3;1;2;1;4];;
let zamenjaj sez = 
	let final = ref sez in
	let rec zam1 sez = match sez with
  | prvi::drugi::r when(drugi < prvi) -> [drugi] @ zam1 ([prvi] @ r)
  | g1::g2::r -> [g1] @ zam1 ([g2]@r)
	| g1::[] -> [g1]
	| [] -> []
	in
	for i=0 to List.length sez -1 do
			final := zam1 !final;	
	done;
	!final

(*Vzorci in prenos parametrov*)
(*se ponavalja vec ali manj ali pa je skoda cajta delat ... psii ce hoces da kj nardim*)
(*sklad 20 naloga*)

let preveriIEmpty sez = match sez with
	| [] -> true
	| _ -> false;;
let rec eraseLast sez = match sez with
	| [] -> failwith "andns"
	| g::[] -> []
	| g::r -> [g] @ eraseLast r

let sklad sez =
	let tmp = ref [] in
	let rec premik sez = match sez with
	| [] ->  preveriIEmpty !tmp
	| g::r when(g='(') -> tmp:= !tmp @ [g]; premik r
	| g::r when(g=')' && List.length !tmp != 0) -> if (List.nth !tmp ((List.length !tmp)-1) = '(') 
		then begin tmp:= eraseLast !tmp; premik r end else false
	| g::r -> false
	in
	
	premik sez
	
(* _a*b*_ 23 naloga*)
let preveri niz = 
	let final =  ref true in
	let prevA = ref false in
	let najdenA = ref false in
	let najdenB = ref false in
	for i=0 to String.length niz-1 do
		if(!final) then begin
  		if (!najdenA) then begin
  			if (niz.[i] = 'b') then begin
					najdenB := true; 
					najdenA := false; 
				end
  			else if (niz.[i] = 'a') then najdenA := !najdenA
  			else final := false	
			end	else if (!najdenB) then begin
				if (niz.[i] = 'a') then begin
					final := false
				end else if(niz.[i] = 'b') then najdenB := !najdenB
				else najdenB := false
  		end
			else if (niz.[i] = 'b') then final := false
			else if (niz.[i] = 'a' && !prevA = false) then begin 
				najdenA := true;
				prevA := true;
			end	else if (niz.[i] = 'a') then final := false
		end
	done;
	!final


(* _a*b*_ 27 naloga*)
let rec cnta sez = match sez with
| [] -> []
| 'a'::'a'::'a'::r -> [3]@ cnta r
| 'a'::'a'::r -> [2]@ cnta r
| 'a'::r -> [1] @ cnta r
| _::r -> [0] @ cnta r



(*Primeri izpitnih nalog (funkcije)*)
(*2 naloga*)

let rec neki f sez = match sez with
| [] -> []
| (a,b)::r when(f b) -> [a] @ neki f r
| (a,b)::r -> neki f r
(*3 naloga*)

let cikli m n =
	let final = ref [] in
	for i=0 to n do
		for j=0 to m do
			final:= !final @ [j]
		done;
	done;
	!final






