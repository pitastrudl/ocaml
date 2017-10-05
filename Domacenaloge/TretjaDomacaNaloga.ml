(*prepdostavljamo, da nimamo tro besednih ali vec imen, drugace bo kazin*)

(*abstrakten razred*)
class virtual clovek ime priimek spol  =
 object
  val mutable ime = (ime : string)
	val mutable priimek = (priimek : string)
	val spol = (spol : char)
end;;

(*ljudi*)

class moski ime priimek =
	object 
	inherit clovek ime priimek 'm'
	method izpisiImePriimek =  ime ^" " ^ priimek
	method popraviPriimek novPriimek = priimek <- novPriimek
	method getpriimek = priimek
	end;;

class otrok ime priimek spol =
	object 
	inherit clovek ime priimek spol
	method izpisiImePriimek =  ime ^" " ^ priimek
	method popraviPriimek novPriimek = priimek <- novPriimek
	end;;

class zenska ime priimek =
	object 
	inherit clovek ime priimek 'z'
	method izpisiImePriimek =  ime ^" " ^ priimek
	method popraviPriimek novPriimek = priimek <- novPriimek
	end;;


let klemen = new otrok "janez" "kekec" 't';;
klemen#izpisiImePriimek;;
klemen#popraviPriimek "bedanc";;
klemen#izpisiImePriimek;;

let jan = new moski "seageate" "barracuda";;
jan#izpisiImePriimek;;
jan#popraviPriimek "caviar";;
jan#izpisiImePriimek;;

let wd_red = new zenska "raid" "10";;
wd_red#izpisiImePriimek;;
wd_red#popraviPriimek "5";;
wd_red#izpisiImePriimek;;


class druzina imedruzine moz zena =
 object 

	initializer 
	zena#popraviPriimek (String.sub 
	moz#izpisiImePriimek 
	(String.index moz#izpisiImePriimek ' ')  
	( (String.length moz#izpisiImePriimek) - (String.index moz#izpisiImePriimek ' ') ) 
)
	val imedruzine = (imedruzine : string)
	val moz = (moz : moski)
	val zena = (zena : zenska)
	val mutable otroci = ([] :otrok list) 
	method  dodajOtroka otrok = otroci <- otrok::otroci;otrok#popraviPriimek moz#getpriimek
	method izpisi = (moz#izpisiImePriimek,zena#izpisiImePriimek),List.map (fun x -> x#izpisiImePriimek) otroci
	end;;

let druzinca1 = new druzina "kekci" jan wd_red;;

(*sexamo*)
druzinca1#dodajOtroka klemen;;
druzinca1#izpisi;;
let andrej = new otrok "jack" "danielsss" 's';;
druzinca1#dodajOtroka andrej;;
druzinca1#izpisi;;


