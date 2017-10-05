[9;6;7;1;3;5];;

zadeva da se preslika v nterico
([<] [>=])

let pivotDeli pivot sez =
	((List.filter (fun x -> x < pivot) sez),
	(List.filter (fun x -> x >= pivot)sez))
	;;

let sez1 = [9;6;7;1;3;5];;

pivotDeli 5 sez1;;

let rec quick_sort sez = match sez with
| [] -> []  (*vrne prazn seznam*)
| g::r -> let (sezm,sezv) = pivotDeli g r  (*sez manjsih in seznam vecjih ,se uporabi glavo kot rep oz *)
in 
(quick_sort sezm) @ [g] @ (quick_sort sezv);;

quick_sort sez1;;

(*se splaca puscat pivot*)


(*polja*)

let polje1 = [|7;5;4;4;3;8|];;

let polje2 = Array.make 8 't';;

polja so indeksirana, navadni oklepaji pri poljih

polje1.(5);;

polje2.(6) <- 's';;
polje2.(6);;

Array.length polje2;;

//matrike

let matrika1 = Array.make_matrix 4 2 1;;

matrika1.(1).(0) <- 7;;
matrika1;;

(*naloge*)
(* polje je imperativne strkturje, seznam je *)
(* rekurzivne oblike*)
(* in ko definiras imperativno polje definiras na ramu in ne gre povecovat kar tako*)
(* nobo polje treba dat in dat notri polje*)
(* *)
(* bomo naredil obe resitvi*)

let polje1 = [|8;9;6;7;2;4|] ;;
let polje2 = [|7;3;1|];;
(*notri bodo samo osmice*)
(* 4 spremeljivke kaj vomo delal pri vtavljanju*)
(*na pl3 na i-to mesto iz plx iz jtega mesta *)
(* j se bo spreminjal, in ko pridemo do zadnega elementa bo j neki naredu*)
(*ko je j enako 5 prepisem cez in grem na nasledno polje in je to zaustavitveni pogoj *)
(**)
(* *)
(* *)
(* *)
(* *)
let zdruziPol pol1 pol2 = 
	let pol3 = Array.make (Array.length pol1 + Array.length pol2) pol1.(0)
	in
	let rec vstavi vPl3 i izPlx j = match j with
	| _ when (j = ((Array.length izPlx) -1)) -> vPl3.(i) <- izPlx.(j) (*se ustavi in da zadnji element v polje*)
	| _ -> vPl3.(i) <- izPlx.(j); vstavi vPl3 (i+1) izPlx (j+1)
	in
	vstavi pol3 0 pol1 0; vstavi pol3 (Array.length pol1) pol2 0;pol3;;


zdruziPol polje1 polje2;;

let zdruziPol pol1 pol2 = 
	let pol3 = Array.make (Array.length pol1 + Array.length pol2) (pol1.(0))
	in
	for i=0 to ((Array.length pol1) -1) do pol3.(i) <- pol1.(i) done;
	for i=0 to ((Array.length pol2) -1) do pol3.(i+ Array.length pol1) <- pol2.(i) done;
	pol3;;


let kreirajP n = 
	let polje = Array.make n 1 in
	let rec vstavi pol i  =  match n with 
	| _ when (i = n-1 ) -> polje.(i) <- n
	| _ -> polje.(i) <- (i+1);vstavi  pol (i+1)	
	in
	vstavi polje 0; polje;;

kreirajP 8;;




let kreirajP n = 
	let polje = Array.make n 1 in
	for i=0 to (n-1) do polje.(i) <- (i+1) done; polje;;
kreirajP 8;;


let kreirajPMN n m= 
	let polje = Array.make (m-n+1) 1 in
	let rec vstavi pol i  =  match n with 
	| _ when (i = (m-n) ) -> polje.(i) <- m
	| _ -> polje.(i) <- (n+i);vstavi  pol (i+1)	
	in
	vstavi polje 0; polje;;

kreirajPMN 7 16;;


let kreirajPMN n m= 
	let polje = Array.make (m-n+1) 1 in
	for i=0 to (m-n) do polje.(i) <- (i+n) done; polje;;

kreirajPMN 7 16;;


let sez1 = [5;7;3;8;9]

let pretvoriSezP sez = 
	let polje = Array.make (List.length sez) (List.hd sez) in
	let rec vpisi pol i s = match s with
	| [] -> polje
	| g::r -> pol.(i) <- g; vpisi pol (i+1) r
	in
	vpisi polje 0 sez;;

pretvoriSezP sez1;;


let pretvoriSezP sez = 
	let polje = Array.make (List.length sez) (List.hd sez) in
	for i=0 to ((List.length sez)-1) do polje.(i) <- (List.nth sez i) done;
	polje;;
pretvoriSezP sez1;;



let pretvoriPoljeS pol =
	let rec vstavi pol1 i sez = match i with
	| _ when (i=(Array.length pol1)-1) -> sez @ [pol1.(i)]
	| _ -> vstavi pol1 (i+1) (sez @ [pol1.(i)])
	in
	vstavi pol 0 [];;

pretvoriPoljeS polje1;;



let pretvoriPoljeSFor pol =
	let seznam = ref [] in
	for i=0 to ((Array.length pol)-1) do seznam := !seznam @ [pol.(i)] done;  (*! je stara vrednost*)
!seznam;;

pretvoriPoljeSFor polje1;;


let vrniMN mat m n = mat.(m-1).(n-1);;



