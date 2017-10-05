type seznam = Nil | Elm of int*seznam;;
type seznam = Nil | Elm of int*seznam;;

let sez1 = (Elm (3, Elm (4, Elm (5, Nil))));;

let rec prestej sez = match sez with
| Nil -> 0
| Elm (x,y) -> 1 + prestej y;;

prestej sez1;;

Elm (9, sez1);;

let sez2 = ref Nil;;

let vstavi sez el = sez := Elm (el, !sez);;

vstavi sez2 8;;

!sez2;;

let rec zmnozi sez = match sez with
| Nil -> 1
| Elm (x,y) -> x * zmnozi y;;

zmnozi !sez2;;


type 'a pseznam = Nil | Elm of 'a * 'a pseznam;;

let sez1 = (Elm (3, Elm (4, Elm (5, Nil))));;

let sez2 = (Elm (3., Elm (4., Elm (5., Nil))));;

type 'a bin_tree =
Empty
| Node of 'a bin_tree * 'a * 'a bin_tree ;;

let drevo1 = (Node (Node (Empty,2,Empty),4,Node (Empty,5,Empty)),
6,
Node(Node (Empty,7,Empty),8,Empty));;

type 'a tree = {
mutable cont:'a;
mutable left:'a bin_tree;
mutable right:'a bin_tree
}
and 'a bin_tree =
Empty
| Node of 'a tree ;;

let drevo3 = Node {cont=6;left= Node {cont= 4; left = Empty; right = Empty}; right= Node {cont= 8; left = Empty; right = Empty} };;



(* 12.5.2015*)



(* Seznam *)

(* Definicija tipa *)
type plist = Nil | Elm of int * plist;;

(* deklaracija spremnlivke*)
let a = Elm(1,Elm(2,Elm(3,Elm(4,Nil))));;

(* Preštevanje elementov *)
let rec stevilo l = match l with Nil -> 0 | Elm(v,t) -> 1 + stevilo t;;

(* Fakulteta v seznamu *)
let rec produkt l = match l with Nil -> 1 | Elm(v,t) -> v * produkt t;;




(* Parametrizirano binarno drevo *)
type 'a bin_tree =
	Empty
	| Node of 'a bin_tree * 'a * 'a bin_tree ;;




(* Preslikava drevesa v seznam *)
let rec list_of_tree = function
	Empty -> []
	| Node(lb, r, rb) -> (list_of_tree lb) @ [r] @ (list_of_tree rb) ;;


(* Funkcija vstavljanje v drevo *)
(* let rec insert x = function *)

let rec insert x drevo = match drevo with
	Empty -> Node(Empty, x, Empty)
	| Node(lb, r, rb) -> if x < r then Node(insert x lb, r, rb)
	else Node(lb, r, insert x rb) ;;

let drevo = ref Empty;;

drevo := insert 8 !drevo;;

!drevo;;

(* Funkcija pretvorbe seznama v drevo *)
let rec tree_of_list = function
	[] -> Empty
	| h :: t -> insert h (tree_of_list t) ;;

let drevo1 = tree_of_list [6;6; 8; 6; 8;2;9;4;3;7;1];;

list_of_tree drevo1;;

(* Funkcija sortiraj seznam, preko pretvorbe v drevo*)
let sort x = list_of_tree (tree_of_list x) ;;


(* Iskanje elementa v drevesu *)



(* Implementacija sklada *)


(* definicija tipa *)
type 'a stack = { mutable ind:int; size:int; mutable elts:'a array };;

(* inicializacija sklada *)
let init_stack n = {ind=0; size=n; elts =[||]} ;;

let sklad1 = init_stack 7;;

let sklad2 = init_stack 7;;

(* funkcije za delo s skladom *)
exception Stack_empty ;;
exception Stack_full ;;
let pop p =
    if p.ind = 0 then raise Stack_empty
    else (p.ind <- p.ind - 1; p.elts.(p.ind)) ;;

let push e p =
    if p.elts = [||] then
    (p.elts <- Array.create p.size e;
    p.ind <- 1)
    else if p.ind >= p.size then raise Stack_full
    else (p.elts.(p.ind) <- e; p.ind <- p.ind + 1) ;;

let top p=
	if p.ind = 0 then raise Stack_empty
	else (p.elts.(p.ind -1)) ;;
	

push 9 sklad1;;

push 'a' sklad2;;

pop sklad1;;

top sklad1;;

sklad1;;

sklad2;;


(* Uporaba *)
let p = init_stack 4 ;;

push 1 p ;;

for i = 2 to 5 do push i p done ;;

p ;;

pop p ;;

pop p ;;



(* Sklad z neomejenim št elementov*)
type 'a stack =
    {mutable ind:int ; mutable size:int ; mutable elts:'a array} ;;

(* popravimo push funkcijo *)
let push e p =
    if p.elts = [||]
    then
        begin
            p.elts <- Array.create p.size e;
            p.ind <- 1
        end
    else if p.ind >= p.size then
        begin
            let nt = 2 * p.size in
            let nv = Array.create nt e in
            for j=0 to p.size-1 do nv.(j) <- p.elts.(j) done ;
            p.elts <- nv;
            p.size <- nt;
            p.ind <- p.ind + 1
        end
    else
        begin
            p.elts.(p.ind) <- e ;
            p.ind <- p.ind + 1
        end ;;

(* Primeri *)
let p = init_stack 4 ;;

for i = 1 to 5 do push i p done ;;

p ;;





(*delal 18.5*)

type 'a pplist = Nil | Elm of 'a * 'a pplist;;  (*parametrizirano to je ta crtica a, poemni da karj vstavimo bo kasneje doloceno kaj bo, crtica a mora bit povsod *)
let sez1 = Elm(3,Elm(6,Elm(9,Nil)));;

let sez2 = Elm(3.,Elm(6.,Elm(9.,Nil)));; 

(*prevest v drevo*)

type bin_tree = Empty | Node of 'bin_tree * int  * bin_tree;;

type 'a bin_tree = Empty | Node of 'a bin_tree * 'a * 'a bin_tree ;;  

let drevo1 = 
	Node (Node (Node(Empty,1,Empty),3,Node(Empty,5,Empty))
	,10, 
	Node(Empty,12,Node(Empty,16,Empty)));;   

let drevo1 = Node (Node(Node (Empty,1,Empty),3, Node(Empty,5,Empty)) ,10, Node(Empty,12,Node(Empty,16,Empty)));;  (*isti k zgori*)

let drevo1 = Node (Node(Node (Empty,[1.],Empty),[3.], Node(Empty,[5.],Empty)) ,[10.], Node(Empty,[12.],Node(Empty,[16.],Empty)));; 
(*to so vozlisca od drevesa in njihove vrednosti,glej drevo v zvesku*)

let rec isci drevo el = match drevo with
| Empty -> false (* ce ni elementa oziroma nismo najdl*)
| Node (lpd,v,dpd) when (el = v) -> true (* levo poddrevo, vozlisce, desno poddrevo*)
| Node (lpd,v,dpd) -> isci lpd el || isci dpd el ;;(* ce najde enga ali drugega je true*)
																							  
isci drevo1 122;;
isci drevo1 [12.];;   


(*vstavljanje, binarno iskalno drevo da pomeni da je urejeno po velikosti. v iskalnem drevesu ni enakih pojavitev*)
let rec vstavi drevo el = match drevo with  (*vstavljamo elemente v drevo*)
| Empty -> Node(Empty,el,Empty)  (*novo vozlisce*)
| Node(lpd,v,dpd) when (el > v) -> Node(lpd,v,vstavi dpd el) (*ko dobimo drevo, in ko je element vecji od vozlisca, bo rekurzivno slo dol v poddrevo desni in vstavilo *)
| Node(lpd,v,dpd) when (el < v) -> Node(vstavi lpd el,v,dpd) ;; (*to je za levo stran oz za vsa drevesa*)


let rec vstavi drevo el = match drevo with  (*ce je se karkoli oz isti element*)
| Empty -> Node(Empty,el,Empty)  
| Node(lpd,v,dpd) when (el > v) -> Node(lpd,v,vstavi dpd el) 
| Node(lpd,v,dpd) when (el < v) -> Node(vstavi lpd el,v,dpd)  
| Node(lpd,v,dpd) -> Node(lpd,v,dpd);;

vstavi drevo1 11;; (*izpise drevo ven,wtf je to drevo, kill me now please, zaenkrat ne popravljamo vrednosti, samo izpisujeo rezultat*)


let drevo3 = ref Empty;;  (* bomo popravil vrednosti*)

drevo3 := vstavi !drevo3 10; !drevo3;; (* unit ker je referenca, vstavimo vozlisce,se vrne vrednost*)
drevo3 := vstavi !drevo3 3; !drevo3;; (*vrstni red vstavljanja je vazen*)
drevo3 := vstavi !drevo3 5; !drevo3;; 
drevo3 := vstavi !drevo3 6; !drevo3;; 
drevo3 := vstavi !drevo3 12; !drevo3;;   

(*pretvarnajnje seznama v drevo*)
let sez1 = [5;7;2;3;1;8;50;12;60];;

let rec vDrevo sez = match sez with
| [] -> Empty (*prazne seznam, kreiras drevo empty?*)
| g::r -> vstavi (vDrevo r) g;;  (*rekurzivno seznam drevesa*)

vDrevo sez1;; (* 60 je zadnji eleemnt,,gre od uzadi pa nazaj, prvo vozlisce je 60, vsako stevilo gre posebi po drevesu in gleda kam ga da
levo manjsi, desn ovecji*)

(*iz drevesa v seznam, vozlisce v oglate oklpeaje*)

let rec vSeznam drevo = match drevo with
| Empty -> []
| Node(lpd,v,dpd) -> (vSeznam lpd) @ [v] @ (vSeznam dpd);; (*rekurzivno do empty, bo vedno vzel eno "vejisce" npr pri 60 bo vzel prazen za ldesnga, pa 12ko za levo*)

let drev3 = vDrevo sez1;;

vSeznam drev3;;(* urejena struktura je zaradi vstavi funkcije*)


(*sklad je se top, je zapis, so polja v zapisu, npr polje tipa int, *)



