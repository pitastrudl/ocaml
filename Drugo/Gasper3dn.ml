type 'a tree = {
mutable vrednost:'a;
mutable levo:'a bin_tree;
mutable desno:'a bin_tree
}
and 'a bin_tree =
Empty
| Node of 'a tree ;;

let rec dodajVDrevoG drevo sp = match drevo with
| Empty -> Node {vrednost = sp; levo = Empty; desno = Empty}
| Node {vrednost = vr; levo = l; desno = d} -> Node {vrednost = vr; levo = l; desno = dodajVDrevoG d sp}

 let rec dodajVDrevoGI drevo sp = match drevo with
| Empty -> Node {vrednost = sp; levo = Empty; desno = Empty}
| Node {vrednost = vr; levo = l; desno = d} when (sp < vr) -> Node {vrednost = vr ;levo = dodajVDrevoGI l sp; desno = d}
| Node {vrednost = vr; levo = l; desno = d} when (sp > vr) -> Node {vrednost = vr; levo = l; desno = dodajVDrevoGI d sp}
| Node {vrednost = vr; levo = l; desno = d} -> Node {vrednost = vr; levo = l; desno = d}


let rec zdruziDrevesiG drevo1 drevo2 = match drevo1 with
| Empty-> drevo2
| Node {vrednost = vr; levo = l; desno = d} -> dodajVDrevoG (zdruziDrevesiG l (zdruziDrevesiG d drevo2)) vr;;





let rec brisiZDrevesaG drevo sp = match drevo with
| Empty -> Empty
| Node {vrednost = vr; levo = l; desno = d} when (vr != sp) -> Node {vrednost = vr; levo = brisiZDrevesaG l sp ; desno = brisiZDrevesaG d sp}
| Node {vrednost = vr; levo = l; desno = d} -> zdruziDrevesiG l d

let rec drevoMapG f drevo = match drevo with
| Empty -> Empty
| Node {vrednost = vr; levo = l; desno = d} -> Node {vrednost = f(vr); levo = drevoMapG f l; desno = drevoMapG f d}

let rec izpisiG drevo = match drevo with
| Empty -> Empty
| Node {vrednost = vr; levo = l; desno = d} -> Node{vrednost = vr; levo = izpisiG(l); desno = izpisiG(d)}



class ['a] drevo ime =
	object (self)
	
	val mutable ime = (ime: string);
	val mutable struc = (Empty:'a bin_tree);


	method dodajVDrevo el = struc <- (dodajVDrevoG struc el);
	method brisiZDrevesa el = struc <- (brisiZDrevesaG struc el);
	method drevoMap f = struc <- (drevoMapG f struc);
	method izpisi = izpisiG struc;
	method set_struc newstruc = struc <- newstruc;
	method get_struc = struc;
	

	end;;


class ['a] iskalnoDrevo ime =
	object (self)
	inherit ['a] drevo (ime)

	method dodajVDrevo el = struc <- (dodajVDrevoGI struc el);
	method brisiZDrevesa el = struc <- (brisiZDrevesaG struc el);
	method drevoMap f = struc <- (drevoMapG f struc);
	method izpisi = izpisiG struc;
	

	end;;

let rec zdruziDrevesi drevo1 drevo2 = let c = new drevo ("hej") in c#set_struc (zdruziDrevesiG drevo1#get_struc drevo2#get_struc); c;;



(* Jaz bi problem z balanciranim binarnim drevesom resil na sledeci nacin: Najprej bi implementiral v razred drevo stevec, ki bi se povecal*)
(* Za 1 vsakic ko kreiramo vstavimo predmet v drevo, ter bi stel koliko predmetov je v drevesu (pri brisanju bi se seveda zmanjsal za 1)*)
(* Nato bi izbral cisto drugacen pristop za vstavljanje v drevo. Vse node drevesa bi ostevilcil (torej da bi poleg vrednosti nosili se svojo cifro) *)
(* Recimo prvi node bi imel stevilko 1, iz njega bi sla node 2 in 3, iz node 3 bi sla node 6 in 7, in tako naprej. Ko bi ustavljali novo cifro v*)
(* Drevo, bi najprej povecali stevec za 1, nato pa prebrali cifro. To je stevilka vozlisca, kamor moremo vstaviti naso cifro. Sedaj naredimo array*)
(* (Recimo dolzine 30, kar zadostuje za 2**30 elementov - po potrebi povecamo) In vanj shranimo dobljeno cifro na prvo mesto. Sedaj to cifro delimo*)
(* Z dva, ter pogledamo celi del. To shranimo na drugo mesto, ter ponavljamo dokler ne dobimo 1. Takrat shranimo se stevilo 1 v array, ter dobili*)
(* Smo pot od nasega vozlisca do zacetka. Cifro bomo v drevo ustavili tako, da se bomo v obratnem vrstnem redu sprehodili do predzadnje cifre v*)
(* Arrayu (po drevesu) (lahko, zato ker vsak node hrani svojo cifro) in glede na sodost/lihost zadnje cifre se odlocili kam vstavimo naso vrednost*)
(* Ter dodamo se tistemu nodu njegovo stevilko.*)
(* Za nadaljevanje potrebujemo funkcijo, ki zna iti po celemu drevesu, ter vse vrednosti shraniti v nek seznam. Tako pri brisanju storimo le naslednje*)
(* Drevo pretvorimo v seznam njegovih vrednosti, iz njega izbrisemo (lahko ze pri vstavljanju v seznam) doloceni element, nato pa iz seznama s*)
(* pomocjo zgornje funkcije za vstavljanje samo vstavimo vrednost po vrednost v seznam. Torej pri brisanju vedno ustvarimo novo drevo, ampak *)
(* tokrat brez tiste vrednosti od prej. (Lahko tudi preverimo ce ima drevo sploh to vrednost, preden "brisemo", ce nismo gotovi ce je element notri)*)
(* Na podoben nacin bi tudi definiral zdruzitev dreves. Pretvorimo prvo drevo v en seznam, drugo v drugi seznam, zdruzimo sezname ter ustvarimo*)
(* novo drevo. (Za stevec ni treba skrbeti, ker se ustrezno povecuje ko vstavljamo elemente)*)
(* Funkciji DrevoMap in Izpisi pa ni treba nic spreminjati za balancirano drevo :) *)


