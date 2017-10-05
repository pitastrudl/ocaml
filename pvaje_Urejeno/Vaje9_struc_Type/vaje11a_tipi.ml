


(*rekurzivne strukture*)
(* *)
(* tipi ki samega sebe klicejo nil je nic?  zaustavitveni pogoj je NIL kar je prazen seznam?*)
(* rekurznivno vzema glavo *)

type plist = Nil | Elm of int * plist;;

let sez1 = Elm(3,Elm(6,Elm(9,Nil)));;

	(**)   
let rec prestej sez = match sez with
| Nil -> 0
| Elm (g,r) -> 1 + prestej r;;

prestej sez1;;

let rec zmnozi sez = match sez with
| Nil -> 1
| Elm (g,r) -> g * zmnozi r;;

zmnozi sez1;;

let vstavi sez elm = Elm (elm,sez);;

vstavi sez1 2;;let rec zmnozi sez = match sez with
| Nil -> 1
| Elm (g,r) -> g * zmnozi r;;

zmnozi sez1;;

let sez2 = vstavi sez1 5;;


let rec zdruzi seza sezb = match seza with
| Nil -> sezb 
| Elm (g,r) -> Elm (g, zdruzi r sezb);;
(**)

zdruzi sez1 sez2;; 