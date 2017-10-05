(*
Tipi, Rekurzivni tipi in Zapisi
1. deklariraj tip placilo, ki je ali Gotovina:int ali Kartica:string*string*int.
• Naredi nekaj nakupov in jih shrani v spremenljivke
• Naredi funkcijo, ki vrne znesek nakupa, ne glede na način plačila
• Naredi funkcijo, ki sešteje znesek nakupov. Nakupi naj bodo podani v obliki seznama

Primer uporabe:
let nakupi = [nakup1;nakup2;nakup3;nakup4];;*)

type placilo = Gotovina of int | Kartica of string*string*int;;

let nakup1 = Gotovina 10;;
let nakup2 = Kartica ("Maestro","Debetna",20);;

let znesek nakup = match nakup with
| Gotovina x -> x
| Kartica (_,_,x) -> x;;

znesek nakup1;;

let rec vsota nakupsez = match nakupsez with
| [] -> 0
| (Gotovina x)::r -> x + (vsota r)
| Kartica (_,_,x)::r-> x + (vsota r);;

vsota [nakup1;nakup2];;


(*
2. Deklariraj tip oseba, ki je lahko Kralj, Papez, Vitez ali Kmet, za katere velja, da je kralj
samo "Kralj", papež ima obliko imena npr.: Janez Pavel 2, vitez in kmet pa imata samo
ime (brez priimka).
• Definiraj funkcijo, ki zna povedati ali je prva oseba v n-terici nadrejena drugi. Velja
pa: Kralj > Papez > Vitez > Kmet.
Primer uporabe:
nadrejen (Kralj,Papez("Janez","Pavel",2));;
− : bool = true
*)
type osebek = Kralj
            | Papez of string*string*int 
            | Vitez of string 
            | Kmet of string;;

let nadrejen (o1,o2) = match (o1,o2) with
|(Kralj,Kralj) -> false
| (Kralj,_) -> true
| (Papez(_,_,_),_) when o2!=Kralj -> true
| (Vitez _,Kmet _) -> true
| _ -> false;;

nadrejen (Papez ("Janez","Pavel",2),Papez ("Janez","Pavel",2));;
nadrejen (Vitez "Blaz", Kmet "Andrej");;
nadrejen (Papez ("Janez","Pavel",2),Vitez "kke");;
nadrejen (Vitez "Blaz",Kmet "Andrej");;
nadrejen (Kmet "Blaz",Vitez "Andrej");;

(*
∗ 3. Dano je drevo, ki je definirano z naslednjim razredom.
class Drevo {
int elm ;
int vsota ;
Drevo levo = null;
Drevo desno = null;
}
Napiši metodo vsote(), ki v vsakem vozlišču izračuna vsoto vseh vozlišč poddrevesa.
Izpiši poddrevo, ki vsebuje vozlišča z vsoto > 10
*)

type drevo = Elm of int | Node of drevo*int*drevo;; 


class drevo (ime,struc) =
val mutable a=int;

;;

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


(*
4. Dan imamo tip
type geo_objekt = Tocka | Premica | Krog | Trikotnik
in seznam geometrijskih objektov, npr.
letgl : geo_objekt list = [ Tocka ; Tocka ; Premica ; Krog ; Krog ]
Napiši funkcijo, ki prešteje število pojavitev posameznega 
geometrijskega objekta v seznamu:
val prestej : geo_objekt list −> geo_objekt −> int = <fun>
*)
type geo_objekt = Tocka | Premica | Krog | Trikotnik;;
let gl = [Tocka;Tocka;Premica;Krog;Krog];;

let stej sez = 
	let t = ref 0 and p = ref 0 and k = ref 0 and tr = ref 0
	in
let rec count sez = match sez with
| [] -> [!t;!p;!k;!tr]
| g::r when g=Tocka -> t :=!t +1;count r
| g::r when g=Premica -> p :=!p +1 ;count r
| g::r when g=Krog -> k :=!k +1 ;count r
| g::r when g=Trikotnik -> tr :=!tr +1 ;count r
in count sez;;

stej gl;;

(*za posameznega*)
let prestej tip sez  = 
let rec pres tip sez n = match sez with
| [] -> n
| g::r -> if g=tip then pres tip r (n+1) else pres tip r n
in pres tip sez 0;;

prestej Tocka gl;;


(*
∗ 5. Dano je drevo, ki vsebuje dve vrsti elementov. 
Definirano je z naslednjo podatkovno
strukturo:
type ( ’a , ’b ) drevo =
Prazno
| Vozlisce aof’a∗(’a,’b) drevo list;;
| V o z l i s c e b o f ’ b ∗ ( ’ a , ’ b ) drevo l i s t ; ;
Napiši funkcijo razcepi : (’a,’b) drevo -> ’a list * ’b list, 
ki prepiše vse elemente Vozliscea
v prvi seznam in vse elemente Vozlisceb v drugi seznam.
*)
type ('a,'b) drevo =
	Prazno 
	| Vozliscea of 'a * ('a,'b) drevo list
	| Vozlisceb of 'b * (('a,'b) drevo) list;;

let d3 = Vozliscea (5,[Vozlisceb ("b", [Prazno]); Vozlisceb ("c",[Prazno])]);;

let razcep drevo = 
	let seza = ref [] and sezb = ref []
	in
	let rec razcp drevo = match drevo with
	| (Prazno) -> (!seza,!sezb)
	| Vozliscea (a,b) -> seza := !seza@[a]; elem b
	| 
	in
	let rec elem sez = match sez with
	| [] -> ()
	| g::r -> razcp g; elem r
	in
	razcp drevo;;


(*goran*)
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





(*
∗ 6. Dana je n-terica, ki predstavlja naslednje podatke študent-a/ke:
- ime in priimek,
- letnik (1 - prvi, 2 - drugi, 3 - tretji),
- povprečna ocena (6 - zadostno; 7 -dobro; 8,9 - prav dobro; 10 - odlično) in
- hobiji.
Tip n-terice je predstavljen z naslednjo definicijo.
# type student=string∗int∗int ∗ ( string list);;
type student=string∗int∗int∗stringlist
Napiši funkcijo izpis: student -> unit, ki pretvori podatke o študentu v tekstovno obliko.
Poskusite uporabiti vzorce, da bi dobili kratko in razumljivo kodo.
Primer:
# izpis ( " Tone Novak " , 20,1,8,("kolesarjenje" ));;
Študent Tone Novak o b i s k u j e p r v i l e t n i k .
Njegova povrečna ocena j e prav dobro .
H o b i j i s t u d e n t −a / ke s o : k o l e s a r j e n j e .
*)

type student =  string*int*int*(string list);;
let peter = ("Peter Kozlovic",2,7,["tek";"plavanje";"kolesarjenje";"droge"]);;

let izpis student = 
	let letnik n = match n with
	| 1-> "prvi letnik"
	| 2-> "tretji letnik"
	| 3-> "drugi letnik"
	| 4-> "cetrti letnik"
	in
	let ocena n = match n with
	| n when n>6 -> "nezadostno"
	| 6-> "zadostno"
	| 7->"dobro"
	| 8->"zelo dobro"
	| 9->"odlicno"
	| 10->"odlicno"
	in
	let rec hobiji sez = match sez with
	| [] -> ""
	| g::r -> g ^ "";hobiji r
	in
let izp (a,b,c,d) = match (a,b,c,d) with
| (a,b,c,d) -> print_string ("Študent" ^ a ^ "obiskuje" ^ (letnik b) ^ "." ^ "\n")
| (a,b,c,d) -> print_string ("Njegova povprecna ocena je "  ^ (ocena c) ^ ".\n")                   
| (a,b,c,d) -> print_string ("Njegovi hobiji so: " ^ (hobiji d) )
in izp student;;  

izpis peter;;


izpis ("Peter Kozlovic",2,7,["tek";"plavanje";"kolesarjenje";"droge"]);;




(*od markota*)
(*Dana je n-terica, ki predstavlja naslednje podatke študent-a/ke:
- ime in priimek,
- letnik (1 - prvi, 2 - drugi, 3 - tretji),
- povprečna ocena (6 - zadostno; 7 -dobro; 8,9 - prav dobro; 10 - odlično) in
- hobiji.
Tip n-terice je predstavljen z naslednjo definicijo.
# type s t u d e n t = s t r i n g ∗ i n t ∗ i n t ∗( s t r i n g l i s t ) ; ;
type s t u d e n t = s t r i n g ∗ i n t ∗ i n t ∗ s t r i n g l i s t----------------------------------------------------------------------*)
type ime = string;;
type letnik = Prvi | Drugi | Tretji;;
type povpOcena = Zadostno | Dobro | PravDobro | Odlicno;;
type hobiji = string list;;

type student = ime*letnik*povpOcena*hobiji;;
 
(*-goran---------------------------------------------------------------------*)

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


(*
7. Dan imamo seznam, definiran z rekurzivnim podatkovnim tipom izraz.
type i z r a z =
Nil
| Stevilo of int ∗ izraz
| Oper o f c h a r ∗ i z r a z ; ;
Izraz vsebuje aritmetične izraze, ki so lahko sestavljeni iz števil (Stevilo) in operacij (Oper).
Dovoljene operacije so plus ’+’ in minus ’-’. Predpostavljamo, da izrazi opisujejo pravilne
aritmetične izraze.
Napiši funkcijo ovrednoti : izraz -> int, ki izračuna vrednost izraza.
Primer:
e = 10 + 5 − 3
# l e t e = S t e v i l o ( 1 0 , Oper ( ’ + ’ , S t e v i l o ( 5 , Oper ( ’ − ’ , S t e v i l o ( 3 , N i l ) ) ) ) ) ; ;
v a l e : i z r a z = S t e v i l o ( 1 0 , Oper ( ’+ ’ , S t e v i l o
( 5 , Oper ( ’ − ’ , S t e v i l o ( 3 , N i l ) ) ) ) ) ; ;
# ovrednoti e ; ;
− : i n t = 12
*)

type izraz =
	Nil
	| Stevilo of int * izraz
	| Oper of char * izraz;;
let e = Stevilo (10, Oper('+',Stevilo(5, Oper ('-',Stevilo (3, Nil)))));;
let rec ovrednoti e = match e with
| Stevilo (x,Oper(a,y)) when (a='+') -> x + ovrednoti y
| Stevilo (x,Oper(a,y)) when (a='-') -> x - ovrednoti y
| Stevilo (x,Nil) -> x;;








(*
∗∗ 8. Dana je definicija parametričnega binarnega drevesa. Parameter tipa bindrevo je spremen-
ljivka tipa ’a, ki predstavlja tip elementov v listih drevesa.
type ’ a b i n d r e v o = L i s t o f ’ a | Drevo o f ’ a b i n d r e v o ∗ ’ a b i n d r e v o ; ;
Napiši funkcijo izpis: ’a bindrevo -> (’a -> bool) -> unit, ki izpiše vse liste drevesa za
katere vrne drugi parameter funkcije izpis - funkcija tipa ’a -> bool - vrednost true.
Napiši funkcijo obrni: ’a bindrevo -> ’a bindrevo, ki obrne vhodno drevo tako, da vsa leva
poddrevesa zamenja z desnimi poddrevesi.
*)
type 'a bindrevo = List of 'a | Drevo of 'a bindrevo * 'a bindrevo

(*
9. Definiraj tip slika s katero predstavimo sliko sestavljeno iz 100x100 točk. Vsaka točka je
predstavljana z intenziteto in barvo (obe vrednosti predstavimo s celim številom).
Predpostavljamo, da imamo že napisano funkcijo pika :
int*int -> bool, ki pove ali
je na dani koordinati pika. Barva pike ni pomembna.
Nekje na sliki je narisan krog z radijem 5. Napiši funkcijo poisci :
slika -> (int*int),
ki poišče središče kroga.
*)
type 'a grm =
	Nic
	| Ena of 'a * 'a grm
	| Dva of 'a * 'a grm * 'a grm;;










(*
∗∗ 10. Dan je tip grm, ki je definiran na sledeč način:
# type ’ a grm =
Nic
| Ena o f ’ a ∗ ’ a grm
| Dva o f ’ a grm ∗ ’ a ∗ ’ a grm ; ;
Napiši funkcijo dolzinevej :
’a grm -> unit, ki izpiše dolžine vej grma po principu
levo-v-globino.
*)










(*
11. Urejevalnik besedil ima tekst predstavljen z dvo-dimenzionalno matriko znakov.16
4. TIPI, REKURZIVNI TIPI IN ZAPISI
(a) Definiraj tip tekst, ki predstavlja dvo-dimenzionalno matriko znakov velikosti 100x1000
(1000 vrstic po 100 znakov)..
*)










(*
∗ ∗ ∗ 12. Napiši funkcijo zamenjajNavpicno :
tekst -> string -> string -> tekst,
ki poišče vse pojavitve niza (2. parameter) vertikalno v tekstu urejevalnika (1. pa-
rameter) in jih zamenja z drugim nizom (3. parameter). Predpostavimo, da sta niza
enako dolga.
*)










(*
13. Dan je tip 2seznam s katerim je predstavljen dvojno povezan seznam.
type 2 seznam = {
value : int ;
mutable next : 2 seznam ;
mutable prev : 2 seznam
} ;;
Cela števila hranimo po naraščajočem vrstnem redu. Napiši funkcijo
dodaj :
2seznam -> int -> 2seznam
ki doda število (2. parameter) na pravo mesto v seznam.
*)

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








(*
14. Urejevalnik besedil ima tekst predstavljen z dvo-dimenzionalno matriko znakov tipa tekst.
type tekst = char array array;;
(a) Napiši funkcijo poisci :
tekst -> unit, ki izpiše vse pojavitve vzorca "ban*ana"
kjer "*" pomeni poljubno število poljubnih znakov.
(b) Napiši funkcijo poisci1 :
tekst -> unit, ki izpiše vse besede teksta, ki se končajo
z nizom "ana".
*)










(*
∗ ∗ ∗ 15. Definiraj parametriziran tip ’a seznamVrednosti z uporabo zapisov. Zapis naj ima dve
komponenti: vrednost tipa ’a in kazalec na naslednji zapis v seznamu oz. prazen seznam.
Napiši funkcijo dolzina :
’a seznamVrednosti -> int, ki prešteje število vrednosti v
seznamu.
*)










(*
∗∗ 16. V programskem jeziku Ocaml imamo definirano podatkovno strukturo binarno drevo.
type ’ a drevo = {
mutable l e v o : ’ a bin_drevo ;
mutable v o z l i s c e : ’ a ;
mutable desno : ’ a bin_drevo
}
and ’ a bin_drevo = Prazen | V o z l i s c e o f ’ a drevo ; ;
Napiši funkcijo, ki izpiše vsa vozlišča tretjega nivoja drevesa, če obstaja.*)










