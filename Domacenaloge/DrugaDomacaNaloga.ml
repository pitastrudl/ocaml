let polje = Array.make 10 1;;
Random.int 200;;
let rPolje polje = 
	for i=0 to ((Array.length polje)-1) do polje.(i) <- Random.int 20 done;;
rPolje polje; polje;;	
	

	


		(*-----------------------------------------*)
let fill array piece start i =
	for i=start to i do
		piece.(i) <- array.(i)
		done;piece;;		

let e = [|10;2;3;4|];;
let b = [|0;0|];;
let g= fill e b 0 1;;



let rec mergesort array = 
	if(Array.length array > 1) then
		let i = ref 0 and j = ref 0 and k = ref 0 in
		let mid = Array.length array / 2 in
		let a = mid in
		let b = Array.length array - mid in
			let lefthalf = fill array (Array.make a 0) 0 a in 
			let righthalf = fill array (Array.make b 0) (a+1) (Array.length array) in
			mergesort lefthalf ;
			mergesort righthalf;
			
			while !i < Array.length lefthalf && !j < Array.length righthalf do
				if(lefthalf.(!i) < righthalf.(!j)) then 
					begin
				  	array.(!k) <- lefthalf.(!i);
						i := !i +1;
					end
				else 
					begin	
						array.(!k) <- righthalf.(!j)
						j := !j + 1
					end
				k := !k +1;
			done;
			
			while !i < Array.length lefthalf do
				array.(!k) <- lefthalf.(!i)
					i := !i +1;
					k := !k +1;
			done;
			
			while !j < Array.length righthalf do
				array.(!k) <- lefthalf.(!j)
					j := !j +1;
					k := !k +1;
			done
					
	;;
