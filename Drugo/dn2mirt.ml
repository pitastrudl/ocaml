let pomnilnik = (Array.make 100 '0');;

let a = ref [(100,0)];;

let malloc l= 
  let f = fst(List.hd(!a)) in
  let s = snd(List.hd(!a)) in
    a := (f-l, s+l)::!a;;

let free p l =
  for i=p to p+l-1 do 
    pomnilnik.(i) <- '0';
  done;;

let repair j l= 
  for i = j to j+l-1 do
    if(i+l < Array.length pomnilnik) then
      (pomnilnik.(i) <- pomnilnik.(i+l);
       pomnilnik.(i+l) <- '0')
    else
      pomnilnik.(i) <- '0';
  done;;

let rec repair2 j l sez = match sez with
  | [] -> []
  | g::r when (snd(g) < j) -> [g]@repair2 j l r
  | g::r when (snd(g) == j) -> []@repair2 j l r
  | g::r -> [((fst g)+l, (snd g)-l)]@repair2 j l r;;

repair2 3 4 !a;;

let sez1 = [];;
let sez2 = [2];;
let sez3 = sez1@sez2;;

let explode s =
  let rec explode2 i l =
    if (i < 0) then l
    else explode2 (i-1) (s.[i] :: l)
  in
    explode2 ((String.length s) -1) [];;

let rec insert2 sez i= match sez with
  | [] -> ()
  | g::r -> pomnilnik.(i) <- g; insert2 r (i+1);;

let insert bes = 
  if(String.length bes > fst(List.hd(!a))) then
    print_string  "Pomnilnik je poln, izprazni nekaj prostora"
  else
    let sez = explode bes in
    let i = snd(List.hd(!a)) in
      insert2 sez i;malloc (String.length bes);;


let rec find bes sez = match sez with
  | [] -> ()
  | g::r -> let sez = ref (explode bes) in
      let b = ref true in
      let stop = ref ((snd g)+(String.length bes)-1) in
        if (!stop >= 100) then stop := 99;
        for i = (snd g) to !stop do
          if(List.hd(!sez) != pomnilnik.(i)) then find bes r;
          sez := List.tl(!sez);
        done;
        if(!b != true) then find bes r
        else free (snd g) (String.length bes); repair (snd g) (String.length bes); a := repair2 (snd g) (String.length bes) (!a);;


let rec find bes sez = match sez with
  | [] -> ()
  | g::r -> let sez = ref (explode bes) in
      let b = ref true in
        for i = (snd g) to (snd g)+(List.length !sez)-1 do
          if(List.hd(!sez) != pomnilnik.(i)) then b := false;
          sez := List.tl(!sez);
        done;
        if(!b != true) then find bes r
        else free (snd g) (String.length bes); repair (snd g) (String.length bes); a := repair2 (snd g) (String.length bes) (!a);;


let delete bes =
  let sez = List.tl(!a) in
    find bes sez;;

delete "btat";;
find "btat" (List.tl(!a));;
check "btat" 3;;
repair 3 4;;

insert "bat";;
insert "btat";;
insert "bat";;


for i=0 to 4 do 
  insert "sad"; 
	done;;
pomnilnik.(3) <- 'b';
pomnilnik.(0);;
pomnilnik.(1);;
pomnilnik.(2);;
pomnilnik;;

free 2 5;;
!a;;
List.hd(!a);;


(*v2?00 drugi fajl*)
let pomnilnik = (Array.make 100 '0');;

let a = ref [(100,0)];;

let malloc l= 
  let f = fst(List.hd(!a)) in
  let s = snd(List.hd(!a)) in
    a := (f-l, s+l)::!a;;

let free p l =
  for i=p to p+l-1 do 
    pomnilnik.(i) <- '0';
  done;;

let repair j l= 
  for i = j to j+l-1 do
    if(i+l < Array.length pomnilnik) then
      (pomnilnik.(i) <- pomnilnik.(i+l);
       pomnilnik.(i+l) <- '0')
    else
      pomnilnik.(i) <- '0';
  done;;

let rec repair2 j l sez = match sez with
  | [] -> []
  | g::r when (snd(g) < j) -> [g]@repair2 j l r
  | g::r when (snd(g) == j) -> []@repair2 j l r
  | g::r -> [((fst g)+l, (snd g)-l)]@repair2 j l r;;

repair2 3 4 !a;;

let sez1 = [];;
let sez2 = [2];;
let sez3 = sez1@sez2;;

let explode s =
  let rec explode2 i l =
    if (i < 0) then l
    else explode2 (i-1) (s.[i] :: l)
  in
    explode2 ((String.length s) -1) [];;

let rec insert2 sez i= match sez with
  | [] -> ()
  | g::r -> pomnilnik.(i) <- g; insert2 r (i+1);;

let insert bes = 
  if(String.length bes > fst(List.hd(!a))) then
    print_string  "Pomnilnik je poln, izprazni nekaj prostora"
  else
    let sez = explode bes in
    let i = snd(List.hd(!a)) in
      insert2 sez i;malloc (String.length bes);;


let rec find bes sez = match sez with
  | [] -> ()
  | g::r -> let sez = ref (explode bes) in
      let b = ref true in
      let stop = ref ((snd g)+(String.length bes)-1) in
        if (!stop >= 100) then stop := 99;
        for i = (snd g) to !stop do
          if(List.hd(!sez) != pomnilnik.(i)) then find bes r;
          sez := List.tl(!sez);
        done;
        if(!b != true) then find bes r
        else free (snd g) (String.length bes); repair (snd g) (String.length bes); a := repair2 (snd g) (String.length bes) (!a);;


let rec find bes sez = match sez with
  | [] -> ()
  | g::r -> let sez = ref (explode bes) in
      let b = ref true in
        for i = (snd g) to (snd g)+(List.length !sez)-1 do
          if(List.hd(!sez) != pomnilnik.(i)) then b := false;
          sez := List.tl(!sez);
        done;
        if(!b != true) then find bes r
        else free (snd g) (String.length bes); repair (snd g) (String.length bes); a := repair2 (snd g) (String.length bes) (!a);;


let delete bes =
  let sez = List.tl(!a) in
    find bes sez;;

delete "btat";;
find "btat" (List.tl(!a));;
check "btat" 3;;
repair 3 4;;

insert "bat";;
insert "btat";;
insert "bat";;


for i=0 to 4 do 
  insert "sad"; 
	done;;
pomnilnik.(3) <- 'b';
pomnilnik.(0);;
pomnilnik.(1);;
pomnilnik.(2);;
pomnilnik;;

free 2 5;;
!a;;
List.hd(!a);;