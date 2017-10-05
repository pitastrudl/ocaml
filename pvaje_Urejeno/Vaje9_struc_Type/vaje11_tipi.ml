(*bomo delal svoje tipe*)
(* zapisi so nterices*)
(* z type dolocmo tipa, in pol ime spremenljivke?*)
(* *)
type trikotnik = {a:int;b:int;mutable c:int};;(*to je nov tip, mesto v zapisu so crke, in tip mesta je int*)

let trik1 ={a=4;b=4;c=5};;  (*nemors met manj kot 3 mesta ali vec*)
let trik2 = {a=5;b=5;c=5};;
trik1;;
trik1.a;;
trik1.c <- 7;; (*not mutable pomeni da polju ne moremo porpavt, treba dat mutable c*, mors vse vedno predefinirat*)
(*sssssssss *)

type oseba = {
	ime:string;
	mutable priimek:string;
	lrojstva:int;
	spol:char;
	mutable zStan:bool;
	};;
	
	let janez = {ime="Janez";	priimek="Novak";	lrojstva=1991;	spol='m';
		zStan=false
		};;

		let ana = {	ime="Ana";		priimek="Novosel";		lrojstva=1999;		spol='z';
			zStan=false
		};;
ana.priimek;;
(*nov zapis da je druigna da porosimo*)

type druzina = { (*oseba tip ker so ana in janez tipa osebe,bomo dodal ovi k strngu imenas,
 bomo nardil mutable da se priimek spremeni*)
	moz:oseba; zena:oseba;ime:string;
	}
		let lara = {  (*laro snismo redefeniral ko smo nardil mutable*)
			ime="Lara";	priimek="Novosel";		lrojstva=1998;	spol='z';
			zStan=false
		};;

let poroci moski zenska = 
		if (moski.zStan = false && zenska.zStan = false) then 
			(moski.zStan <- true;zenska.zStan <- true;zenska.priimek <- zenska.priimek ^ " " ^ moski.priimek;
			{moz=moski;zena=zenska;ime=moski.priimek ^ "ovi";})
		else failwith "Nekdo je ze porocen!";;

let druzinica1 = poroci janez ana;;	
let druzinica1 = poroci janez lara;;   (*ko na novo definiras globalni tip, je isti tip ampak ima drugace lastnosti. npr ce laro das notri *)

druzinica1.moz.zStan;;

(*druga stvar*)

(*to ni zapis, neki druzga, tam imamo nterice, tukaj imamo neki druzga!?!?!**)
(* tip definiramo, nimamo polj in nemormo dostoopdat gor *)
(* *)
type koordinate = float*float;;
let koper:koordinate = (45.5,13.7);;
type barva = Rdeca;;  (*velke zacetnice so konstrktorji,barva je tip*)

Rumena;;
type barva = Rumena;;
Rumena=Rdeca;;
Rdeca=Rumena;;  (*neki glede definicije se povozi?*)

type barva = Rdeca | Rumena | Zelena | Modra | Bela | Crna;;
type material = Bombaz | Jeans | Kasmir | Usnje | Poliester;;
type velikost = XXS | XS | S | M | L | XL | XXL | XXXL;;
type obleka = barva*material*velikost;;
let majica:obleka = (Rdeca,Bombaz,M);;  (*ce obrnes bo errror npr*)
let anorak:obleka = (Modra,Poliester,XXL);; 

(*konstruktor z parametrom*)

(*zvezdica je nterica*)
type barva1 = RGB of int*int*int | BW of int | CMYK of int*int*int*int;;

let modra = RGB (10,20,200);;
let rumena = CMYK (0,0,200,100);;
let bela = BW 255;;

(*naloga gotovisek?na,int je stevilo, strng je katera kartica, banka pa stevilo, vrnt treba zne, das seznam nakupov in vrnes kaj ste zapravl*)

type placilo = Gotovina of int | Kartica of string*string*int;;

let burek = Gotovina 3;;
let hlace = Kartica ("Maestro","Abanka",130);;
let sladoled = Gotovina 2;;
let ovitek = Kartica ("VisaElectron","NLB",7);;
let jufka = Gotovina 4;;

let znesek nakup = match nakup with  (*nakup je tipa placilo*)
| Gotovina x -> x
| Kartica (_,_,x) -> xs

znesek burek;;
znesek hlace;;

let seznamNakupov = [burek;hlace;sladoled;ovitek;jufka];;
let rec poraba sez = match sez with
| [] -> 0 
| g::r -> znesek g + poraba r;;

poraba seznamNakupov;;

(*druga naloga*)
type oseba =
	| Kralj
	| Papez of string*string*int
	| Vitez of string
	| Kmet of string;;

let jenadrejen (os1,os2) = match os1 with
| Kralj when (os2 != Kralj) -> true
|	Papez (a,b,c) when (os2 != (Papez (a,b,c))) -> false
| Papez _ when (os2 != Kralj) -> true
| Vitez a when (os2 = Kmet a) -> true
| _ -> false;;
	
jenadrejen (Papez ("Janez","Pavel",2),Papez ("Janez","Pavel",2));;
jenadrejen (Vitez "Blaz", Kmet "Andrej");;

	let jenadrejen (os1,os2) = match (os1,os2) with
	| (Kralj, _) when (os2 != Kralj) -> true
	| (Papez _,Papez _) -> false
	| (Papez _, _) when (os2 != Kralj) -> true
	| (Vitez _,Kmet _) -> true
	| _ -> false;;
jenadrejen (Vitez "Blaz",Kmet "Andrej");;




