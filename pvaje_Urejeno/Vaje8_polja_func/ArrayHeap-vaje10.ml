(*delo z spominom se od prej?*)
(* delete max je da spodnjegavzame in ga da na vrh, rekurznivno ga potopi?*)
(* replace  treba izvest ali ali, recimo da je stirka nad vecjimi, ce je manjsi*)
(* ga je treba potapljat. treba menjat z replaceom, prvo pop pol pa push, je bolsi*)

(* *)

let find-max heap =  Array.get heap 0

(*array refresh*)

let array1 = [| 1; 2; 3; 4; 5 |];;

let array2 = Array.make 7 'z';;

let str1 = "banana";;

str1.[0];;

array1.(4);;

let mat1 = Array.make_matrix 6 7 1;;
mat1
mat1.(1).(3) <- 6;;

mat1;;

array1.(4) <- 8;;

array1;;


Array.length array1;;

let array3 = [| 3;6;4;1;3|];;

array1;;

(*------------------------- heap --------------------------*)


let find-max heap = Array.get heap 0

let push heap a = 
	let (heap1,idx) = ((Array.append heap [|a|]), size heap) in 
		print_int idx ; shift_up heap1 idx

let delete_max heap = 
	let (heap1,idx) = (swap heap ((size heap) -1) 0, size heap -2) in 
		shift_down heap1 0

(*replace: pop root and push a new key. More efficient than pop followed by push.
*)

let create_heap = [||]

let heapify a = Array.sort (fun a b -> compare b a) a

let merge heap1 heap2 = 
		let arr = Array.append heap1 heap2 in 
		let comparegt = (fun a b -> compare b a) in 
		Array.sort comparegt arr; arr

(*test*)

let h0 = [|550;4;30;395;5;15|];;
let h1 = [|50;40;300;35;25;105|];;
merge h0 h1

let size heap = Array.length heap

let is_empty heap = (heap = [||])

(*test*)
is_empty heap


let update heap i val_i = 
	let old_val=Array.get heap i in 
	let heap1 = (Array.set heap i val_i; heap) in
	print_int old_val ;
		if old_val > val_i then shift_down heap1 i 
		else shift_up heap1 i

(*test*)
let heap = [|50;40;30;35;25;15|];;
update heap 1 55
update heap 1 29

let delete_last heap = Array.sub heap 0 (size heap -1)

let delete heap i = 
	let last = Array.get heap (size heap -1) in 
		update (delete-last heap) i last

(*two aux functions*)

let swap heap i j = (*zamenajo se vrednosti *)
	let (vi, vj)= (heap.(i), heap.(j)) in 
	heap.(i) <- vj;	heap.(j) <- vi; heap


let order heap i j =  (*dobi vredost i pa j,na teh mestih, in ohrani indeks, drugace pa zamenam indeks, jih na nek nacin obrne*)
	let (val_i,val_j) = ((Array.get heap i) , (Array.get heap j)) in 
		if (val_i >= val_j) then (i,j) else (j,i)
		
(*shift up*)

let rec shift_up heap i =   (* ce bi bla trojka bla vecja bi dobil pi,i...drugace pa i, pi*)
	let pi = (i-1)/2 in match (order heap pi i) with   (*gre gor, ta pi racuna nasledni indeks*)
		| (p,_) when p=pi -> heap  (*ce je p enak pi, izpise kopico, ta p je recimo 1,3 bo to p*)
		| _ -> shift_up (swap heap i pi) pi

(*sift-down: move a node down in the tree, similar to sift-up; *)



let rec shift_down heap i = 
	let l_c = (2*i)+1 in   (* se ne fila sodih, zaradi +1, lahko z pushom na soda?*)
	if size heap <= l_c then heap else (*if no bottom layers exist, ce je velikost manjsa ali enak, kr vrne heap,*) 
	let (c1,c2) = if (size heap) = (l_c+1) then (l_c,l_c) else order heap (l_c) (l_c+1) in (* ce je veliksot kopice enaka lc+1,i=2, lc pa 5,  v order heap sta i=2, j=5,  in i potapljamo dol,*)
		match order heap i c1 with
			|(ii,_) when i=ii -> heap
			| _ -> shift_down (swap heap i c1) c1
			
			
let h = [|6;20;10;10;5;8;1|]
shift_down h 0