(*1. naloga*)
(*ljubo*)
let rec primerjaj n s2 = match s2 with
| [] -> []
| g::r when fst g = fst n -> (fst n,(snd n,snd g)) :: primerjaj n r
| g::r -> primerjaj n r;;

let rec meet s1 s2 = match s1 with
| [] -> []
| g::r -> primerjaj g s2 ::meet r s2
;;


let sez1 =[(1,2);(2,3);(4,5)];;
let sez2 = [(2,4);(4,6)];;

meet sez1 sez2;;


(*js*)

let rec primerjaj (h,sez) = match (h,sez) with
| (h,[]) -> []
| (h,g::r) when fst h = fst g -> (fst h,(snd h,snd g))::(primerjaj (h,r))
| (h,g::r) -> primerjaj (h,g::r);;

let meet sez1 sez2 =
	let rec mt (sez1,sez2) = match (sez1,sez2) with
	|([],x) -> x
	| (g1::r1,sez) -> primerjaj (g1,sez);(mt (r1,sez))
	| (sez,[]) -> sez
	in
	mt(sez1,sez2);;


(*2. naloga*)

encode [|1;1;3;4;4;5|] -> [|(1,2);(3,1);(4,2);5,1)|];;

let prepisi p1 p2 =
	for i = 0 to (Array.length p2) -1 do
		p2.(i) <- p1.(i)
	done;;  

let encode polje =
	let temp = Array.make 1 0 and
	 k = ref 0 
	in
		for i=0 to (Array.length polje )-1 do
			

				if polje.(j) = i then 
					temp.(i) <- (i,!k);
				 	k := !k +1;
					temp = prepisi temp (Array.make i 0)
				else temp = prepisi temp (Array.make i 0);
			done
		done;;
			
			
			
			temp = prepisi temp (Array.make i 0);
			 		  
	
let prepisi p1 p2 =
	for i = 0 to (Array.length p2) -1 do
		p2.(i) <- p1.(i)
	done;p2;;  	
	
let encode polje = 
	let 
	temp = (Array.make 1 (polje.(0),1)) 
  	in
	for i = 1 to (Array.length polje) -1 do
		if (fst temp.(i-1) ) == polje.(i) then
			temp.(i-1) <- (fst temp.(i-1) , snd temp.(i-1))  (*posodobimo nterico*)
			else  
				temp = prepisi temp (Array.make i (0,0))
	done;; 		


let encode a = 
	let i = ref 1 in
	let s = ref 0  in
	let tmp = Array.make (a.length) (0,0) in
	for j=0 to (Array.length a)-1 do
		if (!s != a.(j)) then
			for k=0 to (Array.length a)-1 do
				if (a.(j) = a.(k)) then
					i := !i+1;
					tmp.(j) <- (tmp.(j),!i);
				else
					tmp.(j) <- (tmp.(j),!i)
			done;
		else
			s := a.(j);
			i := 1;
	done;;


(*spet js*)
[|1;1;3;4;4;5|]
[|(1,2);(3,1);(4,2);5,1)|];;

let encode polje = 
	let tmp = Array.make ((Array.length polje)) (polje.(0),1) and
	k = ref polje.(0) and
	current = ref polje.(0)and
	j = ref 0 
		in
	for i = 1 to ((Array.length polje) -1) do
		if polje.(i) = !current then
			k := !k +1;
			tmp.(!j) <- (!current,!k) 
			else 
				 k := 0;
				current := polje.(i);
				j := !j +1 
		done;tmp;;


#trace encode
let test = 	[|1;1;3;4;4;5|]	;;			
encode test;;


let test = [|(1,1)|]
test.(0) <- (2,2)

		(*bomo delal svoje tipe*)
		
(*3*)

type bool_exp = 
	| Val of bool
	| Not of bool_exp
	| And of bool_exp * bool_exp
	| Or of bool_exp * bool_exp
;;

let rec eval e = match e with
| Val x -> x
| Not x -> not ( eval x)
| And (a,b)-> eval a && eval b
| Or (a,b) -> eval a || eval b
;;

let rec eval izraz = match izraz with
| Val exp -> exp
| Not exp -> not ( eval exp)
| And (boolexp1,boolexp2)-> eval boolexp1 && eval boolexp2
| Or (boolexp1,boolexp2) -> eval boolexp1 || eval boolexp2
;;


let bl = Val true
let boolexp = Not bl
boolexp
eval boolexp
		
let bl = Val false
let boolexp = Not bl
eval boolexp

let bl = And (Val false,Val true)
let kuac = Or (Val false, Val false)
eval bl
eval kuac

(*4a naloga*)

class matrika (x,y,elt)=
	object

	val mutable matrika = [||]
		initializer 
	 matrika = Array.make_matrix x y elt
end;;
	
class virtual matriks x y elt  =
 object
	val mutable matrika = [||]
	val len = (x : int)
	val hei = (y : int)
	val elt = (elt : 'a)
	method matrik k = matrika <- k
	initializer
		matrik (Array.make_matrix x y elt)
end;;






