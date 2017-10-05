

let mat1 = [[1;2;3];[4;5;6];[7;8;9]];;
let mat2 = [[12;325;63];[74;85;86];[87;88;89]];;
let mat3 = [1;2;3];;
(*1*)
let listMap sez func = 
let rec sezmap plist fnc = match plist with
| [] -> []
| g::r -> [fnc(g)] @ sezmap r fnc
in 
sezmap sez func;;


(*sodaj dela *)
let rec inc mat f = match mat with
| [] -> []
| g::r -> listMap g f:: inc r f;;


listMap mat2 (fun x -> (x+1));;

inc mat2 (fun x -> (x+1));;

(*2 naloga*)

(*listmap*)

listMap mat2 (fun x -> (List.hd x));;

(*poskus transponiranja*)

let rec trans mat = match mat with
| [] -> []
| _ when List.concat mat = [] -> []
| _ -> listMap mat (fun x -> (List.hd x)) :: trans ( listMap mat (fun x -> (List.tl x)));;

trans mat2;;

[[];[]];;



List.concat [[];[]];;



listMap mat2 (fun x -> (List.tl x))

trans mat2;;

List.map (fun x -> List.hd x) mat2;;


(*namesto rja das skozi list.tl*)
List.map (fun x -> List.tl x) mat2;;



let mat4 = [
	[1;2];
	[4;5];
	[7;8];
];;

[
	[1;4;7];
	[2;5;8];
]

listMap r (fun x -> (List.tl x))

let rec mapt mat = match mat with
| [] -> []
| g::r -> listMap g (fun x -> (List.hd x)) :: mapt (listMap r (fun x -> (List.tl x)));;


(*3*)
let zmnoziMat mat1 mat2 =
  if List.length(List.hd mat1) != List.length mat2 then failwith "nemogoce" 
	else 
  	let mat3= transp mat2 
	in
   let rec mnozi mat= match mat1 with
      | [] -> []
  	  | h::t -> List.hd mat * List.hd mat2 :: List.tl mat
    	|(h::t)::t1 -> listMap mnozi(List.hd mat1)
		in mnozi mat3;;


let rec matmnozi mat1 mat2 = match mat1 with
|[] -> []
| _ when List.concat mat1 = [] -> []
| _ when List.concat mat2 = [] -> []



let rec zmnozi_helper matrika1 matrika2 = match matrika2 with
| [] :: nadrep -> zmnozi_helper matrika1 nadrep
| (podglava :: podrep) :: nadrep -> List.fold_left (+) 0 (List.map2 ( * ) (matrika1) (podglava :: List.map List.hd nadrep)) :: zmnozi_helper matrika1 (podrep::List.map List.tl nadrep)
| [] -> [];;

let rec zmnozi matrika1 matrika2 = match matrika1 with
| glava :: rep when (List.length (List.hd matrika1) != List.length matrika2) -> raise (Failure "matriki nista enakih dimenzij")
| [] :: nadrep -> zmnozi nadrep matrika2
| glava :: rep -> zmnozi_helper glava matrika2 :: zmnozi rep matrika2
| [] -> [];;


let moja_matrika1 = [[1; 2]; [3; 4]];;
let moja_matrika2 = [[4; 7; 8]; [3; 2; 1]];;

zmnozi moja_matrika1 moja_matrika2;;

