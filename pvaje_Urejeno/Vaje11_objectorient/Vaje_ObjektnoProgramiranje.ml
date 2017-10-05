(*namesto konstruktorja? imamo kominikator, z kateirm komniciramo v objekt*)
(*problem je z oklepaji, da vcas ne dela, ker sem metodal odstranil oklepaje*)
(* objektno usmerjeno programiranje *)
(* agregacija,recimo v prostoru, so okna vrata itd. se prav razred gradi vec objektov, oz objekt gradi objekt.objekt objektov, razred razredov.
*)
(* razred hisa *)
class hisa (barva, okna, vrata, sobe, etaze,prebivalci) =
 object
  (* spremenljivke instance, samo inicaliziramo instance, od razreda, ni dejanska spremenljibka*)
	  (* spremenljivke instance, jih defeniramo,mutable != ref, je drugace, za imperativo.*)
	(* to spodaj so kot konstruktorji, oz so definicija spremenljivks *)
  val mutable barva = (barva :string)
  val mutable st_oken = (okna :int)
  val mutable st_vrat = (vrata :int)
  val mutable st_sob = (sobe :int)
  val mutable st_etaz = (etaze :int)
	val mutable st_prebivalci = (prebivalci : int)
  val visina_etaze = 3
  val velikost_sobe = 5
	
	
  (* metode *)
  method get_barva = barva
  method get_st_sob = st_sob
  method set_barva novabarva = barva <- novabarva
  method kvadratura  = velikost_sobe * st_sob
  method visina  = visina_etaze * st_etaz
	method get_prebivalci = st_prebivalci
	method set_prebivalci n = st_prebivalci <- n
 end;;
(* naredimo objekt hisa, do spremenljiv klahko dostopamo samo z metodami, ne z spremenljivkami *)
(* naredimo objekt hisa *)
let nasaHisa = new hisa ("bela", 42, 21, 20, 5,1);;
let sosedovaHisa = new hisa ("zelena", 1, 1, 1, 1,1);;

let visja (hisa1, hisa2) =
 if (hisa1#visina > hisa2#visina) then "prva hisa je visja"
 else "druga hisa je visja";;

visja (nasaHisa, sosedovaHisa);;


(*vaje*)

class vozilo(povphitrost,barva,radio)=
	object
	val mutable povprecna_hitrost = ( povphitrost : int )
	val mutable barva = (barva : string)
	val mutable tip_radija = (radio : string)
	method cas_potovanja razdalja = razdalja / povprecna_hitrost
	end;;

let bus = new vozilo(90,"rdec","bluetooth");;

bus#cas_potovanja 100;;





(* agregacija - mesto agregira hise *)
class mesto ime =
 object
  val ime = (ime :string)
  val mutable st_prebivalcev = 2 (* riko in senad *)
  val mutable st_his = 0
  val mutable stavbe = ([] :hisa list) (*seznam tipa hisa*)
(*prazen seznam, je seznam his, so objekti razreda hisa.b tem eseznamu so objekti tipa hisa*)

  method get_ime = ime
  method get_prebivalci = st_prebivalcev
  method get_hise = st_his
  method get_barve_stavbe = List.map (fun x -> x#get_barva) stavbe
(*seznam barv his bo vrnil, nad vsakim elemntom seznama poslje msg, get barva in izpise barvo*)
	
	method get_hiske = List.map (fun x -> x) stavbe
	
  method visine = List.map (fun h -> h#visina) stavbe
  method dodajHiso hisa =
    stavbe <- hisa::stavbe;
    st_prebivalcev <- (st_prebivalcev + hisa#get_prebivalci);
    st_his <- (st_his+1)
 end;;

let koper = new mesto "Koper";;

koper#get_hise;;

koper#get_prebivalci;;

koper#dodajHiso nasaHisa;;

koper#dodajHiso sosedovaHisa;;

koper#get_hise;;

koper#get_prebivalci;;

koper#visine;;

koper#get_hiske;;

koper#get_barve_stavbe;


(*unit je zato ker samo poravmo vrednost spremenljivkke,da dobi strng in 
	ga prepise, smo povedal da zahtevamo eno spremenljibko *)
(* dedovanje - vikend podeduje od hise *)
class vikend (barva, okna, vrata, sobe, etaze, prebivalce, pozicija) =
 object
  inherit hisa (barva, okna, vrata, sobe, etaze, prebivalce)
  val kul_pozicija = (pozicija : string)
  method kje_si = kul_pozicija
	
end;;

let rogla = new vikend ("bel", 6, 2, 3, 2,2, "Pohorje");;

(* klicemo lahko vse metode nadrazreda *)
rogla#kvadratura;;

rogla#visina;;

rogla#kje_si;;

class pravokotnik (a,b,barva) =
	object
	val mutable a = (a : int)
	val mutable b = (b : int)
	val mutable barva = ( barva : string )
	method get_a = a
	method get_b = b
	method get_barva = barva

	method ploscina = a*b

end;;			 

let p1  = new pravokotnik(3,4,"rdec");;
let p2 = new pravokotnik(3,5,"zelen");;

let ustvari p1 p2= 
	let barv = if (p1#ploscina <= p2#ploscina) then p2#get_barva
	else p1#get_barva in
if (p1#get_a = p2#get_a) then new pravokotnik(p1#get_a, p1#get_b+p2#get_b,barv)
else if(p1#get_b = p2#get_b) then new pravokotnik(p1#get_b, p1#get_a+p2#get_a,barv)
else failwith "ooops";;

let p3 = ustvari p1 p2;;
p3#get_a;;
p3#get_barva;;





(* razred racunalnik *)
class pc (mhz, jedra, ram, disk, hladilnik) =
 object
 val mhz = mhz
 val jedra = jedra
 val ram = ram
 val disk = disk
 val hladilnik = hladilnik
 method poraba  = (hladilnik + (mhz * jedra))/60
 method get_jedra = jedra
end;;

let nasSuperRacunalnik = new pc (3000, 2048, 5000000, 100000000, 512);;

nasSuperRacunalnik#poraba;;

(*to naj bi bil subclass*)
class prenosnik (mhz, jedra, ram, disk, hladilnik, teza) =
 object (self)
 inherit pc (mhz, jedra, ram, disk, hladilnik) as super(*nadrazred*)
 val teza = teza
 method poraba  = super#poraba / 2 (*nadrazredu posljemo sporocilo*)
end;;

let prenosnik = new prenosnik (1500, 2, 2048, 80000, 50, 1);;

prenosnik#poraba;;








(* self in super *)
class pravokotnik a b =
 object
  val a = a
  val b = b
  val mutable barva = ""
  method vrni_a = a
  method vrni_b = b
  method vrni_barva = barva
  method pobarvaj novabarva = barva <- novabarva
  method ploscina  = a * b
  method obseg  = (2*a) + (2*b)
  method izpisiStranice  =
   "stranica a: " ^ string_of_int (a) ^
   " stranica b: " ^ string_of_int (b)
end;;

(* s super klicemo metodo starsa *)
class skatla a b c =
 object (self)
 inherit pravokotnik a b as super
 val c = c
 method povrsina = (super#ploscina + b*c + a*c)*2
 method izpisiStranice =
   super#izpisiStranice ^ " stranica c: " ^ string_of_int c
end;;

(* naredimo pravokotnik *)
let pravokotnik4 = new pravokotnik 5 10;;
pravokotnik4#izpisiStranice;;


pravokotnik4#izpisiStranice;;



(* naredimo skatlo *)
let skatla1 = new skatla 5 6 7;;

skatla1#povrsina;;

skatla1#izpisiStranice;;

(* inicializacija *)
class skatla a b c =
 object (self) (*lahko uporabljamo da klicemo svoje metode*)
 inherit pravokotnik a b as super (*inicalizer se bo sprozu ob kreaciji objekta*)
 initializer print_string ("Naredim skatlo z robovi "
   ^ string_of_int a ^" "^ string_of_int b ^" "
   ^ string_of_int c ^ "\n")(*; ignore(new skatla a b c)*)
 val c = c
 method povrsina = (super#ploscina + b*c + a*c)*2
 method izpisiStranice =
   super#izpisiStranice ^ " stranica c: " ^ string_of_int c
end;;

let skatla2 = new skatla 6 7 8;;


(* primer privatne metode , so dostopne razredu, ne pa objektu*)
class stavek besedilo =
 object (self)
 val besedilo = (besedilo :string)
 method vrni_besedilo = besedilo

 method private explode besedilo =
  let rec expl i l =
   if (i < 0) then l
   else expl (i-1) (besedilo.[i]::l)
  in
 expl (String.length besedilo - 1) []

 method private implode seznam =
  let izpis = String.create (List.length seznam) in
  let rec implode2 i s = match s with
   | [] -> izpis
   | c::s -> izpis.[i] <- c; implode2 (i+1) s in
  implode2 0 seznam

 method obrni_besedilo =
  self#implode(List.rev (self#explode(besedilo)))
end;;

let perica = new stavek "Pericarezeracirep"

perica#vrni_besedilo

perica#obrni_besedilo

perica#explode  (*ne dela ker je private*)

(* abstrakten razred - uporabimo besedo virtual, kot kalup, napovemo metode, ki jih podrazredi ki spadajo v ta nadrazred,*)
(* definirat. kalup *)
class virtual lik barva stranice =
 object
  val mutable barva = (barva :string)
  val st_stranic = (stranice :int)
  method virtual ploscina : int  (*metode brez parametrov, *)
  method virtual obseg : int
	method virtual pobarvaj : string -> unit 
end;;

(* kvadrat mora definirati ploscino in obseg *)
class kvadrat barva a =
 object
 inherit lik barva 4
 val a = (a :int)
 method ploscina = a * a
 method obseg = 4 * a
method pobarvaj newbarva = barva <- newbarva
end;;

(* pravokotnik mora definirati ploscino in obseg *)
class pravokotnik barva a b=
 object
 inherit lik barva 4
 val a = (a :int)
 val b = (b :int)
 method ploscina = a * b
 method obseg = 2 * a + 2 *b
method pobarvaj newbarva = barva <-newbarva
end;;

(* pravokotniTrik mora definirati ploscino in obseg *)

class pravokotnitrik barva k1 k2=
 object
 inherit lik barva 3
 val a = (k1 :int)
 val b = (k2 :int)
 method ploscina = a * b / 2
 method obseg = a + b + (5)
method vrni_a = a
method pobarvaj newbarva = barva <-newbarva
end;;



type 'a tree = {
mutable vrednost:'a;
mutable levo:'a bin_tree;
mutable desno:'a bin_tree
}
and 'a bin_tree =
Empty
| Node of 'a tree ;;

let drevo0 = Empty

let drevo2 = Node {vrednost=4;levo=Empty;desno=Node {vrednost=2;levo = Empty;desno = Empty}};;(*dodamo element 4*)

let drevo1 = Node {vrednost=2;levo= Node{vrednost=1;levo=Empty;desno=Empty};desno=Empty}





(**)
(* ni glih objektov, bolj pomemben zapis, bomo z zapisi anredil*)
(*  vrednost 'a je vozlisce *)
(* *)
(* 1(a) je narobe,,definiraj metodo, ne funkcijo, b je pa funckija*)
(*1, ni binarno iskalno drevo, je drevo, ga damo kamorkoli, ne izbiramo elementov, prvi prsostor, damo kamorkoli.  *)
(* struc je mutable*)
(* d.*)
(* izpisi bo vrnla struc, ena vrstica*)
(* *)
(* 2.*)
(* pazi da se elementi v drevesu ne smejo ponavljati. v drevesih je lahko karkoli*)
(* *)
(* *)
(* *)
(* 3. *)
(* *)
(* *)
(* *)
(* *)
(* *)
(* *)
(* *)
(* *)
(* *)
(* *)
(* *)
(* *)






