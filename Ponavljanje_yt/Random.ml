let i = ref 1;;

!i;;

i := !i +1; !i;;  (*:=pomeni prireditev nove vrednoseti*)

for i=0 to 10 do print_int i; print_string "\n" done;;

for i=10 downto 0 do print_int i;print_string "  "  done;;


let r = ref 1
	in while !r < 11 do
		print_int !r ;
		print_string " ";
		r := !r+1
done ;;


(*
let r = 1 in
	while r < 11 do
		print_int r;
		print_string " "; in
		r = r +1;
	done;;

ne gre *)


let seznamm = [3;6;2;4;5;7];;

let vsota seznam = 
	let rec sestej seznam = match seznam with 
	| [] -> 0
	| g::r -> g + sestej r

in
sestej seznam;;

vsota seznamm;;
	
	
	
	


