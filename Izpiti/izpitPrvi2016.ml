(* 2. *)
type 'a element = E of 'a | L of 'a list;;

let rec print sez = match sez with
	E x -> print_int x
	L s -> print_int (List.hd s); print (List.tl s);;
	

	
(* 3. *)
let calc sez =
	let x = ref (List.hd sez) in
	let rec racunaj s = match (List.tl s) with
		[] -> x := !x
		g1 :: g2 :: r when (g1 = Op PLUS) -> x := !x + g2; racunaj (g2 :: r)
		g1 :: g2 :: r when (g1 = Op TIMES) -> x := !x * g2; racunaj (g2 :: r)
	in
	racunaj sez; !x;;
	
	
(* 4. *)
class ['a] ArrayM (ini: 'a list) =
	object (self)
	inherit ['a] Array (ini: 'a) as super
	method get i = super#get i
	method set x i = x :: (self#get i)
	method del x i = List.filter (fun y -> y != x) (self#get i)
end;;