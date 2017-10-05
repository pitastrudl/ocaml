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


let rec transp mat f = match mat with
| [] -> []
| g::r -> listMap g f:: transp r f;;

transp mat1 (fun x -> x+1);;

(*2*)


let rec trans mat = match mat with
| [] -> []
| _ when List.concat mat = [] -> []
| _ -> listMap mat (fun x -> (List.hd x)) :: trans ( listMap mat (fun x -> (List.tl x)));;

trans mat1;;

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

