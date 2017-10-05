
(*Vsako kopiranje in kraja tujega celotnega dela, 
le delčka ali IDEJE je kaznivo dejanje.
 V primeru kraje je pristojno sodišče v Kopru
  (ce koga dobim je najebu!!). Datoteka 
  "KajZaBogaJeTo.ml - author bananabitch - bananaBitch"
   je namenjena le za vpogled. Prav tako lastnik
    ni nič odgovoren v primeru, da kdo prekrši pravila.
     Po zahtevah naloge je delo samostojno, 
     zato sploh ne pirporocam vpogled v KajZaBogaJeTo.ml 
     oziroma nasploh prenos datoteke.

Don't play with your life!!!*)


type 'a tree = {
	mutable vrednost: 'a;
	mutable levo: 'a bin_tree;
	mutable desno: 'a bin_tree
} and 'a bin_tree = Empty	| Node of 'a tree;;

let rec dodajVDrevo1 dr el = match dr with
| Empty -> Node { vrednost = el; levo = Empty; desno = Empty}
| Node x -> Node{ vrednost=x.vrednost;  levo=dodajVDrevo1 x.levo el; desno = x.desno }

let izpisii drevo = match drevo with
	| Empty -> Empty
	| Node { vrednost = x; levo = l; desno = d;} -> Node { vrednost = x; levo = l; desno = d;};;

(*ker je navpicno nerabis desnge -> vrne vrednost*)
let rec zdruziDrevesi dr1 dr2= match dr1 with
| Empty -> dr2
| Node vozlisce -> dodajVDrevo1 (zdruziDrevesi vozlisce.levo dr2) vozlisce.vrednost ;;
(*za vsak slucaj popravljeno -> zapise v prvega*)
let rec zdruziDrevesi dr1 dr2= match dr2 with
| Empty -> ()
| Node vozlisce -> dr1#dodajVDrevo vozlisce.vrednost; zdruziDrevesi dr1 vozlisce.levo; zdruziDrevesi dr1 vozlisce.desno;; 

(*zdruziDrevesi banana nutella#getStruc;; (*it works!!!*) *)

(*ker je navpicno spet isce le spodnje with l=empty and r=emtpy*)
(*something is misssing!!*)
(*lahko si privoscimo to ker mamo samo navpicno drevo*)	
	
let mapiraj f dr = match dr with
| Empty -> Empty
| Node vozlisce -> Node { vrednost = f vozlisce.vrednost; levo = (mapiraj f vozlisce.levo); desno = (mapiraj f vozlisce.desno)}	

class ['a] drevo ime=
	object(self)
  	val mutable ime = (ime:string)
  	val mutable struc = (Empty:'a bin_tree)
  	method getStruc = struc;
  	method getIme = ime;	
  	method brisi a = struc <- brisii struc a;  
  	method drevoMap a = struc <- (mapiraj a struc);
  	method dodajVDrevo el = struc <- (dodajVDrevo1 struc el);
  	method izpisi = izpisii struc;
	end;;

let rec dodaj drevo a = match drevo with
	| Empty -> Node {vrednost=a ; levo = Empty ; desno = Empty}
	| Node vozlisce when(( vozlisce.vrednost = a )) -> Node vozlisce
	| Node vozlisce when(( vozlisce.vrednost < a )) -> Node { vrednost=vozlisce.vrednost ; levo = vozlisce.levo ; desno = dodaj vozlisce.desno a }
	| Node vozlisce -> Node { vrednost=vozlisce.vrednost ; levo = dodaj vozlisce.levo a ; desno = vozlisce.desno  }	;;

class ['a] iskalnoDrevo ime=
	object (self)
	inherit ['a] drevo ime  as super
	method dodajVDrevo a = struc <- dodaj struc a
	method getStruc = struc;
	end;;




(*test files*)


let banana = new drevo "Banana";;
let nutella = new drevo "Nutella";;
banana#dodajVDrevo "b1";;
banana#dodajVDrevo "b2";;
banana#dodajVDrevo "b3";;
nutella#dodajVDrevo "n1";;
nutella#dodajVDrevo "nn2";;
nutella#dodajVDrevo "nnn3";;
zdruziDrevesi banana nutella#getStruc;;

banana#izpisi
nutella#izpisi

banana#brisi "nn2"

let banana = new iskalnoDrevo "Banana";;
let nutella = new iskalnoDrevo "Nutella";;
banana#dodajVDrevo 50;;
banana#dodajVDrevo 10;;
banana#dodajVDrevo 100;;
banana#dodajVDrevo 5;;
banana#dodajVDrevo 45;;
banana#dodajVDrevo 20;;
banana#dodajVDrevo 46;;
banana#dodajVDrevo 48;;
banana#dodajVDrevo 47;;
banana#dodajVDrevo 13;;
zdruziDrevesi banana nutella#getStruc;;

(*3 naloga:*)
(* *)
(* TEZKO!!! -> vprašaj me :D*)
(* *)