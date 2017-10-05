(*1.naloga funkcija visjega reda, function ki preslika x -> x+2 ?------f od x = x +2, ne more bit praznih listov, oziroma veje, sta dve veji od enga *)
(* vozlisca ali pa ne *)
(* *)
(* *Dan je tip bdrevo, ki je definiran na naslednji nacin
type bdrevo = Leaf of int | Node of bdrevo * int * bdrevo;;
Napisi funkcijo bapply : bdrevo -> (int -> int) -> bdrevo, ki aplicira funkcijo
(int -> int) na vseh vozliscih drevesa. )
(* *)
(* *)
(* *)
*)
type bdrevo = Leaf of int | Node of bdrevo * int * bdrevo;;


let rec bapply drevo f = match drevo with
| Leaf x -> Leaf(f x) (* ce bo leaf bo funkcijo izvedel, smo dodal se leaf (f x) drugace se nic ne dogaja*)
| Node (lpd,v,dpd) -> Node(bapply lpd f,(f v), bapply dpd f);;

let dl = Node(Node(Leaf 13,4,Leaf 5),10,Node(Leaf 4,12,Leaf 11));;

bapply dl (fun x -> x-1);;


(*2. naloga
Dan je tip 'a struc definiran na naslednji nacin.
type 'a struc =
 Elm of 'a
 | Pair of 'a struc * 'a struc
 | Triple of 'a struc * 'a struc * 'a struc
Napisi polimorficno funkcijo
smap : ('a->'b) -> ('a struc) -> ('b struc),
ki aplicira funkcijo f doloceno s 1. parametrom na vseh komponentah Elm x, ki so del 2.
parametra. Rezultat naj bo struktura tipa 'b struc.
Primer:
# smap (function x -> x+1)
 (Triple (Pair (Elm 1, Elm 2),Elm 3, Elm 4));;
- : int struc = Triple (Pair (Elm 2, Elm 3), Elm 4, Elm 5) 


skor identicna nalgoi prejsi samo strutkura je druga

*)

type 'a struc =
 Elm of 'a
 | Pair of 'a struc * 'a struc
 | Triple of 'a struc * 'a struc * 'a struc;;

let rec smap f str = match str with
| Elm x -> Elm (f x)
| Pair (str1,str2) -> Pair(smap f str1,smap f str2)
| Triple(str1,str2,str3) -> Triple(smap f str1, smap f str2, smap f str3);;

let stru1 = (Triple(Pair(Elm 1,Elm 2),Elm 3, Elm 4));;

smap (function x -> float_of_int (x+1)) stru1;;



type itree = Nil | Node of itree*int*itree;;
let inil1 =  Node(Node(Nil,3,Nil),5,Node(Nil,2,Node(Nil,1,Nil)));;


let sumsub drevo = let vrednost voz = match voz with (*smo naredil potem enkrat, dodali se normalno funkcijo*)	
	|Nil -> 0
	| Node(_,x,_) -> x
  in 
  let rec sumsub1 drevo = match drevo with
  | Nil -> Nil(*prazen*)
  | Node (Nil,v,Nil) ->  Node (Nil,v,Nil) (*pusti pri miru*)
  | Node (Nil,v,dpd) ->  Node (Nil,v + vrednost (sumsub1 dpd),sumsub1 dpd)  (*rekurzija je kot sklad, prvi ki je gor dan bo zadnji npr*)
	| Node (lpd,v,Nil) ->  Node (sumsub1 lpd,v + vrednost (sumsub1 lpd),Nil)
	| Node (lpd,v,dpd) ->  Node (sumsub1 lpd,v + vrednost (sumsub1 lpd) + vrednost (sumsub1 dpd), sumsub1 dpd)
	in sumsub1 drevo;;

sumsub inil1;; (*dela*)


(*3. naloga
Binarno drevo je definirano s tipom itree, ki predstavlja binarno drevo katerega vozlisca
vsebujejo vrednost tipa int.
type itree = Nil | Node of itree*int*itree;;
Napisi funkcijo sumsub : itree -> itree, ki iz vhodnega drevesa konstruira novo drevo,
ki namesto originalnih vrednosti v voliscih vsebuje vsoto vrednosti vozlisc levega in desnega
pod-drevesa ter vrednosti danega vozlisca.
Primer:
# sumsub Node(Node(Nil,3,Nil),5,Node(Nil,2,Node(Nil,1,Nil)));;
- : itree = Node(Node(Nil,3,Nil),11,Node(Nil,3,Node(Nil,1,Nil)))*)



