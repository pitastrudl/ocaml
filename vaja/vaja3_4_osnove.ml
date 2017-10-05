let pomnsezx sez x = List.map (fun y -> y*x) sez;;
let seznam = [1;2;3;10;30;40;50;100;120];;
pomnsezx [1;2;3] 2;;

let pomnsezx sez = List.map (fun y -> y*y) sez;;
pomnsezx [1;2;3];;

let negseznam sez = List.map (fun y -> y*(-1)) sez;;
negseznam [1;2;3];;

let povecaj = fun x -> x + 1;;
let kvadrat = fun x -> x * x;;
let krat2 = fun x -> x * 2;;
let ascii = fun x -> char_of_int(x);;
let gasperfy = fun x -> "gasper";;

List.map povecaj seznam;;
List.map kvadrat seznam;;
List.map krat2 seznam;;
List.map ascii seznam;;
List.map gasperfy seznam;;

List.fold_right ( + ) seznam 5;;

let sezt = [false;true;false;false];;
let sezt2 = [false;false;false;false];;
let sezt3 = [true;true;true];;

let seztrue sez = 
let rec ifseztrue sez = 
	if (sez=[]) then false
	else if (List.hd sez) = true then true
	else ifseztrue(List.tl(sez))
	in ifseztrue(List.filter (fun x -> x=true) sez);;

seztrue sezt;;
seztrue sezt2;;

List.fold_right ( && ) sezt3 true;;

let sodast sez = List.filter (fun x -> (x mod 2 = 0)) sez;;
sodast seznam;;

(*todo beseda*)
sezbes "gasper";;

let explode string =
 let rec explode2 i sez =
  if (i < 0) then sez
  else explode2 (i-1) ((string.[i]) :: sez)
 in
 explode2 ((String.length string) -1) []  ;;

let sss = explode "gasper";;
let aaa = explode "aaa";;

let ifhalf a = List.for_all 
(fun x -> ((int_of_char x) < 78) || ((int_of_char x) < 109)) a;;

ifhalf sss;;
ifhalf aaa;;

let explode string =
 let rec explode2 i sez =
  if (i < 0) then sez
  else explode2 (i-1) ((string.[i]) :: sez)
 in
 explode2 ((String.length string) -1) []

let rec inc mat = match mat with
| [] -> []
| g::r -> inc2 g :: inc r;;

let rec inc2 mat = match mat with
| [] -> []
| g::r -> [g+1] @ inc2 r;;

let mat1 = [[1;2;3];[4;5;6];[7;8;9]];;
let mat2 = [[1;2;3];[4;5;6];[7;8;9]];;
let mat3 = [1;2;3];;
(*1*)
let rec inc mat = match mat with
| [] -> []
| g::r -> inc2 g :: inc r;;

let rec inc2 mat = match mat with
| [] -> []
| g::r -> [g+1] @ inc2 r;;
(*sodaj dela *)

let listMap sez func = 
let rec sezmap plist fnc = match plist with
| [] -> []
| g::r -> [fnc(g)] @ sezmap r fnc
in 
sezmap sez func;;

let rec inc mat f = match mat with
| [] -> []
| g::r -> listMap g f:: inc r f;;

listMap mat3 (fun x -> (x+1));;

inc mat2 (fun x -> (x+1));;

(*2*)
let popravi2 sez =
	let rec naredi seznam tmp = match seznam with
	| [] -> []
	| g1::g2::r when g1 = g2 -> naredi ([g2]@r) ([g1]@tmp)
	| g1::g2::r -> ([g1]@tmp)::(naredi ([g2]@r) [])
	| g::r -> ([g]@tmp)::(naredi r [])
	in
	naredi sez [];;
popravi2 ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e"];;



