(* vmesnik *)
module type SEZNAM =
 sig
  val sestej : int list -> int
  val obrni : 'a list -> 'a list
  val je_palindrom : int list -> bool
 end;;

(* implementacija modula , smo dodal :SEZNAM in nima ta seznam funkcije vrni, ce ne prepusemo vmesnika potem dela. *)
module Seznam:SEZNAM =
 struct

let rec sestej seznam =
 if (seznam = []) then 0
 else (List.hd seznam) + sestej (List.tl seznam)

let rec vrni seznam n = match seznam with
 | [] -> 0
 | _ when (n<=0) -> 0
 | g::r when (n=1) -> g
 | g::r -> vrni r (n-1)

let rec obrni seznam = match seznam with
 | [] -> []
 | g::r -> (obrni r) @ [g]

let je_palindrom seznam =
 let rec vrni seznam n = match seznam with
 | [] -> 0
 | _ when (n<=0) -> 0
 | g::r when (n=1) -> g
 | g::r -> vrni r (n-1) in
 let dolzina = List.length seznam in
 let rec primerjaj i j = match i with
  | _ when (i = j) -> true
  | _ when ((vrni seznam i) != (vrni seznam j)) -> false
  | _ -> primerjaj (i+1) (j-1)
 in
 primerjaj 1 dolzina

 end;;

(* vmesnik za polje *)
module type POLJE =
 sig
  val naredi_matriko : int -> int -> 'a -> 'a array array
  val v_polje : 'a list -> 'a array
 end;;

module Polje =
 struct
let v_polje seznam =
 let rec velikost s = match s with
  | [] -> 0
  | g::r -> 1 + velikost r in
 let polje = Array.make (velikost seznam) (List.hd seznam) in
 let rec vpisi sez i = match sez with
  | [] -> polje
  | g::r -> polje.(i) <- g; vpisi r (i+1) in
 vpisi seznam 0

let naredi_matriko n m element =
 let stolpec = Array.make n element in
 let matrika = Array.make m stolpec in
 matrika

let zdruzi_polji2 p1 p2 =
 let p3 = Array.make (Array.length p1 + Array.length p2) p1.(0) in
 for i=0 to ((Array.length p1)-1) do p3.(i) <- p1.(i) done;
 for i=0 to ((Array.length p2)-1) do p3.(i+Array.length p1) <- p2.(i) done;
 p3

let naredi_polje_do n =
 let p = Array.make n 1 in
 let rec vpisi polje i = match n with
  | _ when (i = (n-1)) -> polje.(i) <- (i+1)
  | _ -> polje.(i) <- (i+1); vpisi polje (i+1) in
 vpisi p 0; p
 end;;

(* Vmesnik za Nas *)
module type NAS =
sig
val kvadmat : int list list -> int list list 
end;;

module Nas = 
struct
let kvadriraj x = x*x

let kvadsez sez = List.map kvadriraj sez

let rec kvadmat mat =
if ((List.tl mat) = []) then [kvadsez (List.hd mat)]
else (kvadsez (List.hd mat)) :: (kvadmat (List.tl mat))
end;;

module Seznam = (Seznam:SEZNAM);;
module Polje = (Polje:POLJE);;
module Nas = (Nas:NAS);;


Seznam.vrni [5;6;7] 1;;







(*novo*)

module Ocena =
 struct
  type ocena = { mutable vrednost: int; mutable predmet: string }
  let spremeniVrednost m vrednost = m.vrednost <- vrednost
  let spremeniPredmet m predmet = m.predmet <- predmet
  let poglejOceno m = (m.vrednost, m.predmet)
  let ustvari = { vrednost=0; predmet="" }
 end;;

let prog2 = Ocena.ustvari;;

Ocena.spremeniVrednost prog2 10
prog2;;

Ocena.spremeniPredmet prog2 "Programiranje 2"
Ocena.poglejOceno;;
prog2;;






module Ocena =
 struct
  type ocena = { mutable vrednost: int; mutable predmet: string }
  let spremeniVrednost m vrednost = m.vrednost <- vrednost
  let spremeniPredmet m predmet = m.predmet <- predmet
  let poglejOceno m = (m.vrednost, m.predmet)
  let ustvari = { vrednost=0; predmet="" }
 end;;

(* vmesnik za studenta *)
module type STUDENT =
 sig
  type ocena (* implementacija ocene je skrita - abstrakten tip *)
  val poglejOceno : ocena -> int * string
 end;;

(* naredimo modul Student z uporabo vmesnika STUDENT *)
module Student = (Ocena:STUDENT);;

(* vmesnik za profesorja *)
module type PROFESOR =
 sig
  type ocena (* implementacija ocene je skrita - abstrakten tip *)
  val poglejOceno : ocena -> int * string
  val spremeniPredmet : ocena -> string -> unit
  val spremeniVrednost : ocena -> int -> unit
 end;;

(* naredimo modul Profesor z uporabo vmesnika PROFESOR *)
module Profesor = (Ocena:PROFESOR);;

(* naredimo oceno predmeta programiranja2 in shranimo v prog2 *)
let prog2 = Ocena.ustvari;;

(* ne moremo dostopati do ocene! *)
Profesor.spremeniVrednost prog2 7;;



funktorji

module SIS (Ocena:PROFESOR) = (* krajsi zapis za funktor *)
 struct
  type ocene = { mutable ocene: Ocena.ocena list }
  let pokaziOcene m = m.ocene
  let vpisiOceno m ocena = m.ocene <- (ocena :: m.ocene)
  let ustvari = { ocene = [] }
 end;;




