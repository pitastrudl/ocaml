
(*Tipi, Rekurzivni tipi in Zapisi*)

(*Tipi 1*)
type placilo = Gotovina of int | Kartica of (string*string*int);;
let nakup1 = Gotovina 100;;
let nakup2 = Kartica ("Nlb","Koji kurac",13);;
let znesek1 nakup = match nakup with
| Gotovina x -> x
| Kartica (x,y,z) -> z  
let rec znesek nakupi = match nakupi with
| [] -> 0
| g::r -> znesek1 g+ znesek r
let nakupi = [Gotovina 4; nakup2; nakup1];;
znesek nakupi;;
(*Tipi 2*)

type oseba = Kralj | Papez of (string*string*int) | Vitez of string | Kmet of string;;
(*it should work....*)
let nadrejen (prvi,drugi) = match (prvi,drugi) with
| (Kralj, d) when (d != Kralj)-> true
| (Papez _, d) when (d!=(Papez _) && d!= Kralj) -> true
| (Vitez, d) when (d!=(Papez _) && d!=Vitez && d!=Kralj) -> true
| (_,_) -> false;;


(*Tipi 3*)
let rec racunajVsoto x = match x with
| Empty -> 0
| Node a -> a.elm + racunajVsoto a.levo + racunajVsoto a.desno;;
let rec zunanjaVsota dr =match dr with
	| Empty -> Empty
	| Node x -> Node {elm=x.elm; vsota=x.elm + racunajVsoto x.levo+racunajVsoto x.desno; levo=zunanjaVsota x.levo; desno=zunanjaVsota x.desno};;

class drevo x =
	object
		val mutable podDr = (x:podDrevo);
		method getElm = podDr;
		method vsota = podDr <- zunanjaVsota podDr;
	end;;

type child = {
	mutable elm:int;
	mutable vsota:int;
	mutable levo: podDrevo;
	mutable desno: podDrevo;
}
and podDrevo =
	Empty
	| Node of child;;

let banana = new drevo (Node {elm=1;vsota=0; levo=Node {elm=2;vsota=0;levo=Empty;desno=Node{elm=4; vsota=0;levo=Empty;desno=Empty}}; desno=Node {elm=3;vsota=0;levo=Empty;desno=Empty}});;

banana#getElm
banana#vsota


(*Tipi 4*)
type geo_objekt = Tocka | Premica | Krog | Trikotnik;;
let gl = [Tocka;Tocka;Premica;Krog;Krog];;
prestej gl Trikotnik

let rec prestej gl go= match gl with
| [] -> 0
| g::r when(g=go) -> 1+ (prestej r go)
| g::r -> 0 + (prestej r go)


(*Tipi 5*)
(*nwm kaj je tisti list zraven.. ce kdo pogrunta pisi... :P*)
type ('a, 'b) drevo = 
	Prazno
	| Vozliscea of 'a * ('a, 'b) drevo
	| Vozlisceb of 'b * ('a, 'b) drevo

let neki = Vozliscea ("1a",Vozliscea("2a", Vozlisceb ("1b",Prazno))) 
let razcepi drevo = (pripisiA drevo, pripisiB drevo)
let rec pripisiA neki = match neki with
| Prazno -> []
| Vozliscea (x,y) -> [x] @ pripisiA y
| Vozlisceb (x,y) -> pripisiA y

let rec pripisiB neki = match neki with
| Prazno -> []
| Vozlisceb (x,y) -> [x] @ pripisiB y
| Vozliscea (x,y) -> pripisiB y

(*Tipi 6*)
let dobiLetnik l = match l with
| 1 -> "prvi."
| 2 -> "drugi."
| 3 -> "tretji.";;
let dobiOceno o = match o with
| 6 -> "zadostno."
| 7 -> "prav dobor."
| 8 -> "prav dobor."
| 9 -> "prav dobor."
| 10 -> "odlicno." ;;

let dobiHobije h =
	let rec dobiHobije1 h = match h with
  | [] -> []
  | g::r -> [g] @ dobiHobije1 r
	in
	String.concat "" ([String.concat ", " (dobiHobije1 h)]@ ["."])


type student = string*int*int*(string list)

let lepIzpis nekdo = match nekdo with
| (ime, letnik, ocena, hobiji) -> String.concat " " ["Student";ime;"obiskuje";(dobiLetnik letnik);"Ocena:";(dobiOceno ocena);"Hobiji:";(dobiHobije hobiji)]


let matija =  ("Nekdo Primke", 1, 8,  ["Kolo";"Igrice"])
lepIzpis matija

(*Tipi 7*)

type izraz =
	Nil
	| Stevilo of int * izraz
	| Oper of char * izraz;;
let e = Stevilo (10, Oper('+',Stevilo(5, Oper ('-',Stevilo (3, Nil)))));;
let rec ovrednoti e = match e with
| Stevilo (x,Oper(a,y)) when (a='+') -> x + ovrednoti y
| Stevilo (x,Oper(a,y)) when (a='-') -> x - ovrednoti y
| Stevilo (x,Nil) -> x;;
(*Tipi 8 - predolgo navodilo ne da se brati*)
type 'a bindrevo = List of 'a | Drevo of 'a bindrevo * 'a bindrevo
(*Tipi 9 isto sranje kot pri 8mi*)
(*Tipi 10 a to je dve zvezdici vredn?*)
(*ce je to sam sprehod po levi==??(Ena?Dve?) do nic*)
(*kaj je leva?*)
type 'a grm =
	Nic
	| Ena of 'a * 'a grm
	| Dva of 'a * 'a grm * 'a grm;;


(*Tipi 13*)
(*okj mal so zakomplicirali nmaloge :P skoda casa za to.. ni mi pa ratalo ker prvo sploh nwm kako napisat s kazalalci za prev next rezn ce bi refereno uporabljau amoka ne morem ker sta prev pa next tipa sez2 in ne ref...*)
(*btw ne dela... *)
type sez2 = {
	value:int;
	mutable next:taSez;
	mutable prev:taSez;
}
and taSez =
	Empty
	| Node of sez2

let neki = Node {value=10; next=Empty; prev=Empty};

let dodaj vred sez = 
	match sez with
	| Empty -> Node {value=vred; next= Empty; prev=Empty}
	| Node x when (x.value < vred )-> Node {value=vred; next= x; prev=Empty}
	| Node x -> rekurzivnoDodaj x x.next
	in
	let rec rekurzivnoDodaj prev current = match current with
	| Empty -> Node {value=vred; next= Empty; prev=prev}
	| Node x when (x.value <vred) -> Node 
	| Node x when (x.value>vred) -> 
		let nov = Node {value=vred; next= Empty; prev=Empty}
		Node {value=prev.value; prev=prev; next=nov}} 


(***************************************************)
(***************************************************)
(***************************************************)
										(*Razredi*)
(***************************************************)
(***************************************************)
(***************************************************)

(*Razredi 1*)
class prevSredstvo(v) =
	object
		val mutable v = (v:int);
		method racunaj a = v/a (*matematiki poravte neacbo ce je narobe ne da se mi ubadat*)
		
	end;;
let a = new prevSredstvo (100);;
let b = new prevSredstvo (200);;
a#racunaj 10
b#racunaj 10
(*Razredi 2*)