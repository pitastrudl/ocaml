(*Objektno usmerjeno programiranje
1. Naredi razred za opis prevoznega sredstva s katerim prihajaš na fakulteto. Razred naj
vsebuje spremenljivko za povprecno hitrost prevoznega sredstva in metodo, ki prejme
razdaljo, vrne pa cas potreben za prevoz razdalje glede na povprecno hitrost. Naredi dve
instanci razreda in preizkusi metodo.*)
class prevoznosredstvo (povphitrost) =
	object (self)
	val povprecnaHitrost = (povphitrost:int)
	method cas n= 
		let hitrost n= float_of_int(n) /. float_of_int(povphitrost)
		in hitrost n; 
	end
;;


let avto = new prevoznosredstvo (100);;
let ferarri = new prevoznosredstvo (312);;
avto#cas 10;;
ferarri#cas 500;;

(*
2. Naredi razred rastlina/pohištvo/žival/oblačila, ki ima vsaj dve spremenljivki in dve me-
todi. Naredi podrazred tega razreda, ki doda še vsaj eno spremenljivko in dve metodi.
Temu podrazredu dodaj še en podrazred, ki doda še vsaj eno spremenljivko in eno metodo.
*)

class rastlina (velikost,tip)=
	object 
	val velikost = (velikost:int);
	val tip = (tip:string);
	method getTip=tip;
	method getVel=velikost;
	end;;

class roza (velikost,tip,steviloglav) =
	object 
	inherit rastlina (velikost,tip)
	val glave = (steviloglav:int);
	method getglave = steviloglav;
	end;;

let tulipan = new roza (10,"belovečernica",4);;
tulipan#getTip;;
tulipan#getVel;;
tulipan#getglave;;
(*
3. Naredi razred pravokotnik, ki naj ima vsaj 3 lastnosti in 4 metode. Ena izmed metod naj
bo ploščina. Naredi dva objekta tega razreda. Naredi funkcijo, ki dva pravokotnika zlepi
skupaj (po dolžini ali širini) Naredi podrazred kvadrat in popravi metodo ploščina.*)


type barva = Rumena | Bela | Zelena | Modra;;  (*tip ki bomo uporabljali*)

class pravokotnik (a,b,barva) =
	object
	val mutable vrA = (a:int)
	val mutable vrB = (b:int)
	val mutable barva = (barva:barva)  (*drugace bi blo string*)
	method obseg = 2 * vrA + 2 * vrB
	method ploscina = vrA * vrB
	method vrni_a = vrA
	method vrni_b = vrB
	end;;

let pr1 = new pravokotnik (12,50,Rumena)
let pr2 = new pravokotnik (3,50,Zelena)
let pr4 = new pravokotnik (8,6,Modra)

pr1#vrni_b



let zlepi prav1 prav2 = (*si narisi doma*)
	if (prav1#vrni_a = prav2#vrni_a) then 
		new pravokotnik (prav1#vrni_a, prav1#vrni_b + prav2#vrni_b, Bela)(*ce se ujemata dve vrstici, istolezni stranici,zlepi*)
	else if (prav1#vrni_b = prav2#vrni_b )
	 then new pravokotnik (prav1#vrni_a + prav2#vrni_a , prav1#vrni_b, Bela)
	else failwith "Nobena stranica se ne ujema!";;

let pr3 = zlepi pr1 pr4
pr3;;  (*novi objekt*)
pr4;;

class kvadrat (a,barva) =
	object
	inherit pravokotnik (a,a,barva) 
	method ploscina = vrA * vrA  (*je prepisala plosicnvo metodo *)
	end;;




(*
4. Naredi razred računalnik. Naj ima tipične lastnosti, ter nekaj poljubnih metod. Dodaj še
metodo za porabo elektrike. Naredi podrazred prenosnik, ki doda še nekaj lastnosti ter
naj definira svojo metodo za porabo elektrike (uporabi super).*)


class pc (mhz, jedra, ram, disk, hladilnik) =
 object
 val mhz = mhz
 val jedra = jedra (*niso mutable ker jih ne rabimo za spreminjati*)
 val ram = ram
 val disk = disk
 val hladilnik = hladilnik
 method poraba = (hladilnik + (mhz * jedra))/60  (*zarad tega teh operacij bo int oz te value bojo*)
 method get_jedra = jedra
end;;

let nasSuperRacunalnik = new pc (3000, 2048, 5000000, 100000000, 512);;

nasSuperRacunalnik#poraba;;   


class prenosnik (mhz, jedra, ram, disk, hladilnik, teza) =
 object (self)
 inherit pc (mhz, jedra, ram, disk, hladilnik) as super
 val teza = teza
 method poraba = super#poraba / 2
end;;

let prenosnik = new prenosnik (1500, 2, 2048, 80000, 50, 1);;

prenosnik#poraba;;



(*
5. Dan je seznam celih števil.
c l a s s Seznam {
i n t elm = 0 ;
Seznam n s l d n = n u l l ;
}
Napiši metodo, ki obrne seznam.
Primer: vhod: 2->3->9->1 rezultat: 1->9->3->2*)




(*
6. Dan je razred MojaIzjema.
c l a s s MojaIzjema e x t e n d s E x c e p t i o n {
p u b l i c MojaIzjema ( ) {
}
p u b l i c MojaIzjema ( S t r i n g msg ) {
s u p e r ( msg ) ;
}
}
Implementiraj nov razred MetanjeIzjem z metodo vrziLoviVrzi(), ki
vrze izjemo MojaIzjema,
jo ulovi in izpise "Izjema v razredu MetanjeIzjem",
ponovno vrze izjemo, ki je primerek razreda Exception in
jo ulovi v metodi main() razreda MetanjeIzjem.
*)









(*
7. Definiraj razred Semafor, ki implementira enostaven semafor z dvemi lučmi: rdečo in
zeleno. Stanje semaforja spreminjamo z metodama: rdeca() in zelena(). Uporabniki lahko
hkrati dostopajo do semaforja, zato je potrebno zagotoviti varen dostop do kritične sekcije.
Implementiraj razred Semafor v jeziku Ocaml.
∗ 8. Imamo dve vrsti vrste:
FIFO vrsta, kjer vstavljamo elemente na začetku in jemljemo na koncu, in
FILO vrsta, kjer je element, ki je prvi vstavljen zadnji vzet iz vrste.
Implementiraj opisani vrsti s hierarhijo razredov:
Katere razrede bo potrebno definirat?
Kako bodo razporejene podatkovne strukture po razredih za implementacije vrste na osnovi
polja?
Napiši implementacijo metod: vstavi() in odvzemi().
∗ 9. Geometrijski objekt je definiran z virtualnim razredom geo, ki vsebuje definiciji virtualnih
metod:
predstavi : string , ki predstavi geometrijski objekt
z nizom znakov , i n
n a r i š i : unit , ki n a r i š e objekt in i z p i š e niz
s katerim j e p r e d s t a v l j e n objekt .
Definiraj virtualni razred geo in razrede točka, krog in premica kot implementacije virtu-
alnega razreda geo.
Uporabi dedovanje kjer je mogoče.
Implementiraj metodi predstavi in narisi za vse tri konkretne razrede. Predpostavi, da
imamo že napisane funkcije:
n a r i s i _ t o c k o : i n t ∗ i n t −> u n i t
n a r i s i _ p r e m i c o : i n t ∗ i n t −> i n t ∗ i n t −> u n i t
n a r i s i _ k r o g : i n t ∗ i n t −> i n t −> u n i t
Uporabi prekrivanje metod in kodo razporedi tako, da bolj specifične metode uporabijo
prekrite metode, kjer je mogoče.
∗ 10. Definirati je potrebno hierarhijo razredov za predstavitev podatkov o študentih v infor-
macijskem sistemu ŠIS. Splošne podatke o študentih predstavimo v razredu Oseba. Bolj
specifične podatke predstavimo v razredih Student, PodiplomskiStudent in Asistent. Asi-
stent je poseben primer podiplomskega študenta.
V razredih bi želeli hraniti naslednje podatke:PROGRAMIRANJE II
19
- ime in priimek,
- naslov,
- tel.stevilka,
- vpisan letnik,
- vpisan program,
- obstoječa izobrazba in
- povprečna ocena.
Definiraj razrede v programskem jeziku Ocaml.
Realiziraj razrede tako, da inicializatorji razreda inicializirajo objekt z začetnimi vre-
dnostmi.
V hierarhiji razredov implementiraj metodo predstavi, ki predstavi vse lastnosti poljubnega
primerka razredov v hierarhiji. Uporabljaj dedovanje in prekrivanje metod!
∗∗ 11. V dvo-dimenzionalnem svetu robotov imamo dve vrsti robotov: x-robota, ki se premika
samo po x-osi in y-robota, ki se premika samo po y-osi. Svet ima dimenzije -10..10 po
x-osi in -10..10 po y-osi.
Premik robota po x-osi implementiramo tako, da prištejemo x-koordinati 1 oz. -1, odvisno
od tega ali se robot premika desno ali levo. Ko robot pride do roba sveta zamenjamo
premik iz 1 v -1 oz. obratno.
Premikanje robota po y-osi je definirano enako kot v primeru premikanja po x-osi le da so
osi zamenjane in se robot premika navzgor in navzdol.
(a) Definiraj razrede s katerimi predstavimo x-robota in y-robota. Definiraj skupen ko-
ren hierarhije razredov robot s katerim predstavimo robote z uporabo abstraktnega
razreda.
(b) V abstraktnem razredu robot definiraj virtualno metodo premaknise. Implementiraj
metodo premaknise v okviru obeh konkretnih razredov.
Nasvet: definiraj čim bolj preprosto rešitev!
12. Definiraj razred Zbirka, ki hrani zbirko celih števil urejeno po vrstnem redu določenim z
uporabo funkcij za dodajanje elementov. Implementiraj naslednje metode:
d o d a j _ z a c e t e k : i n t −> u n i t
brisi_zacetek : int
dodaj_konec : i n t −> u n i t
brisi_konec : int
Nasvet: Za implementacijo razreda Zbirka uporabi čim bolj enostavno podatkovno struk-
turo!20
5. OBJEKTNO USMERJENO PROGRAMIRANJE
Razreda Vrsta (FIFO) in Sklad (FILO) implementiraj kot specializaciji razreda Zbirka.
Definiraj ustrezne metode razredov Vrsta (enqueue in dequeue) in Sklad (push in
pop).
∗ ∗ ∗ 13. Definiraj razred Vozlisce s katerim bo predstavljeno navadno binarno drevo. Vozlišča
drevesa so torej primerki razreda Vozlisce.
(a) Vrednost vozlišča naj bo primerek poljubnega tipa ’a definiranega kot parameter
razreda Vozlisce. Definirati je torej potebno parametriziran razred Vozlišče.
(b) Pod-drevesi danega vozlišča definiraj kot opcijske vrednosti s čimer se izognemo de-
finiciji praznega pod-drevesa.
Napiši metodo, ki vstavi novo vozlišče v skrajno levo vejo drevesa.*)