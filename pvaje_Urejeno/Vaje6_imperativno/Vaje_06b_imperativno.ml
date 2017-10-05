let i = ref 0;;

!i;;

i := 't';;

for i=0 to 10 do 
	print_int i;
	print_string " "	
	done;;

for i=10 downto 0 do print_int i done;;

let r = ref 0 
	in while !r < 11 do 
	print_int !r ; 
	print_string " "; 
	r := !r+1 
	done ;; 

let pivot_split pivot seznam =
  (List.filter (fun y -> y < pivot) seznam,
	 List.filter (fun y -> pivot <= y) seznam);;

let seznam = [7;4;2;3;80;5;6;2;4;8;10];;

pivot_split 6 seznam;;

let rec quick_sort seznam = match seznam with
  | [] -> []
  | g :: r -> let (sez1, sez2) = pivot_split g r
              in
              (quick_sort sez1) @ [g] @ (quick_sort sez2);;

quick_sort seznam;;

let naloga s =
let rec vsota sez = match sez with
| [] -> 0
| g::r -> g + vsota r
in 
(vsota (List.filter (fun x -> x mod 2 = 1) s),
vsota (List.filter (fun x -> x mod 2 = 0) s));;

naloga [6;5;2;7;4;9];;

let naloga s =
	let soda = ref 0 in
	let liha = ref 0 in
	let rec racunaj sez = match sez with
	| [] -> (!liha, !soda)
	| g::r when (g mod 2 = 0) -> soda := !soda + g; racunaj r
	| g::r -> liha := !liha + g; racunaj r
	in
	racunaj s;;

let naloga s =
	let soda = ref [] in
	let liha = ref [] in
	 let rec vsota sez = match sez with
	 | [] -> 0
   | g::r -> g + vsota r
	 in
	let rec racunaj sez = match sez with
	| [] -> (vsota !liha, vsota !soda)
	| g::r when (g mod 2 = 0) -> soda := g :: !soda; racunaj r
	| g::r -> liha := g :: !liha; racunaj r
	in
	racunaj s;;

	
	naloga [6;5;2;7;4;9];;
	
