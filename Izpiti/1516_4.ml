(*prva naloga*)
(* Napiši funkcijo višjega reda
foldx : 'a list -> 'b -> ('a -> 'b -> 'b) list -> 'b,
ki nad seznamom, ki je podan s prvim argumentom, in začetno vrednostjo podano z
drugim argumentom, aplicira funkcije iz seznama podanega kot tretji argument na sledeč način.
Naj bo prvi argument enak [a1,a2,...,an], drugi argument b, in tretji argument enak
[f1,f2,...,fn]. Rezultat dobimo na sledeč način:
f1 a1 (f2 a2 ... (fn an b) ...)*)
let sez = [1;2;3;4;5;6];;
let fsez = [(fun x y -> y);(fun x y -> x);(fun x y -> y)];;

(* v foldu posles seznam funkcij, in v rekurzijo poslesl rep seznama funkcij*)
let rec fold_left f a sez = match sez with
[] -> a
| g::r -> fold_left (List.tl f) ((List.hd f) a g) r;;

(*Definiraj funkcijo
zmesaj : 'a array -> (int*int) array -> 'a array,
ki kot prvi parameter sprejme polje vrednosti tipa 'a, kot drugi parameter pa polje parov s
katerimi so definirane zamenjave elementov. Funkcija naj vrne zakodirano polje.
Polje parov vsebuje pare indeksov s katerimi je definirana zamenjava dveh elementov
vhodnega polja. *)
let rPolje polje = 
	for i=0 to ((Array.length polje)-1) do polje.(i) <- Random.int 20 done;;

let zmesaj polje mesanca = 
	let temp = ref () in
	for i=0 to ((Array.length mesanca)-1) do
			temp := polje.(fst mesanca.(i)) ;
			polje.(fst mesanca.(i)) <- polje.(snd mesanca.(i));
			polje.(snd mesanca.(i)) <- !temp;
	done;polje;;

PROBLEM JE DA NI TKO K BI MOGU BIT

(*Dano je drevo, ki vsebuje dve vrsti elementov in je definirano z naslednjo podatkovno
strukturo:
type ('a, 'b) tree =
 Nil
| Nodea of 'a ('a, 'b) tree list ∗
| Nodeb of 'b ('a, 'b) tree list;; ∗
Napiši funkcijo
razcepi: ('a,'b) drevo -> 'a list * 'b list,
ki prepiše vse elemente Nodea v prvi seznam, ki postane prvi element vrnjenega para,
in vse elemente Nodeb v drugi seznam, ki postane drugi element vrnjenega para*)

type ('a, 'b) tree =
 Nil
| Nodea of 'a ('a, 'b) tree list 
| Nodeb of 'b ('a, 'b) tree list;; 



(*Podan je modul Stack, ki realizira sklad elementov tipa 'a..
module type Stack =
sig
 type 'a t
 exception Empty
 val create: unit -> 'a t
 val push: 'a -> 'a t -> unit
 val pop: 'a t -> 'a
end
Definiraj modul Queue, ki s pomočjo dveh skladov implementira vrsto. Prvi sklad
predstavlja začetek vrste in drugi sklad predstavlja konec vrste. Implementiraj funkciji
enqueue and dequeue !
Namig: če je kateri izmed skladov prazen potem lahko drugega “obrnemo”. *)


tega ne bo, lahko pa naredimo objekt, kar bo kul
