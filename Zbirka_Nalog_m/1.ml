(*1.*)
1.2;;
true;;
"String";;
(true,true);;
(2.0,2,'2');;
['a';'b';'c'];;
((2,2),(2,2));;
(2,(true,('2',()),2.0),2,"2");;
(true,(2,(2.0,()),'2'));;
("2",("2",2),[2;2],true);;
(*2.ovrednoti se true ker prvi false se negira*)
not (1=2) && 1.0 < 2.0;;  (*not negira vse*)
(*3. Deklariraj spremenljivki a:int in b:float s poljubno vrednostjo, ter ju seštej in zapiši rezultat
v spremenljivko c:float.*)
let a = 2 and b = 2.0;;
let c = (float_of_int a) +. b;;
(* 4. Deklariraj spremenlivko c:int, ki bo nosila vrednost seštevka spremenlivk a:int b:int, pri
čemer sta spremenljivki a in b v izrazu deklarirani lokalno.*)

let c = 
	let s = 2 and g = 3
in
	s +g;;
(*5. Z uporabo lokalnih spremenljivk izračunaj a*b*c.*)

let zmnoz = 
	let a = 1 and b = 3 and c = 4
	in
	a*b*c;;
(*6. Deklariraj spremenljivki ime:string in telefon:int ter ju shrani v seznam.*)
let ime = "Arun" and telefon = 123123123;;
let imetelsez = [ime;string_of_int(telefon)];;

(*7. Deklariraj poljubna a:int in b:int nato ju vstavi v if stavek, ki naj vrne večje število.*)
let a = 1 and b = 2;;
let ce a b =
	if a < b then b
	else a;;
ce a b;;

(*8. Deklariraj spremenljivko z imenom malaCrka:char in vanjo shrani poljubno malo tiskano
črko. Nato v eni potezi deklariraj spremenljivko velikaCrka:char, ki shrani veliko tiskano
črko pretvorjeno iz malaCrka. Pomagaj si z ASCII tabelo.*)

let malaCrka = 's';;
let velikaCrka = char_of_int(int_of_char(malaCrka) - 32);;

(*9. Deklariraj dva primera n-teric z dvema elementoma in z uporabo dostopanja do elementa
v paru deklariraj novo n-terico, sestavljeno iz prvega elementa ene n-terice in drugega
elementa druge n-terice.*)

let prvituple = ("Jebes","Vse");;
let drugituple = ("Fucking","fuck");;
let nterica = (fst(prvituple),snd(drugituple));;

(*10. Deklariraj seznama števil od 1 do 5 in od 6 do 10, ju združi in zapiši v nov seznam. Iz
doblenega seznama izpiši glavo in rep.*)

let sez1 = [1;2;3;4;5] and sez2 = [6;7;8;9;10];;
let sez3 =sez1 @ sez2;;
List.hd(sez3);;
List.tl(sez3);;

(*11. Seznamu zgornje naloge dodaj element na začetek in konec.*)

let sez3 = 0::sez3@[11];;

(*12. Napiši program v programskem jeziku Ocaml, ki za dana seznama celih števil vrne unijo.
Pazi na duplikate!
Primer:
[1 ,2 ,4 ,7] , [2 ,4 ,7 ,9]  [1 ,2 ,4 ,7 ,9]*)

let cel1 = [1;2;4;7;2;3;7;3;10] and cel2= [2;4;7;9];;

let unija a b = 
List.sort_uniq compare (List.sort compare (List.concat([a;b])));;
unija cel1 cel2;;


let rec primerjaj element seznam = match seznam with
| [] -> false (*ni notri*)
| g::r -> if element=g then true else primerjaj element r;;

(*let unija a b = *)
	
let unija a b = 
  let rec zdruzi sez = match sez with
  	| [] -> []
  	| g::r when ((primerjaj g r)=true) -> zdruzi r
  	| g::r when ((primerjaj g r)=false) -> [g] @ zdruzi r
in 
zdruzi (a@b);;


unija [3;4;7;9;8] [4;5;6;9;9];;


unija [1;100;1;2;3;2;4;5;5] [2];;



(*∗ 13. V programskem jeziku Ocaml izpiši vse podnize danega niza! Primer kaže tudi možno
zasnovo algoritma.
Primer:
banana
banana, banan, bana, ban, ba, b, anana, anan, ana, an, a,
nana, nan, na, n, ana, an,a,na,n ,a*)

(*moja verzija ki ne dela*)
let pod niz = 
	let c = ref 0 in 
	for i=0 to (String.length niz -1) do
		for j=i to (String.length niz -1) do
			for k=j to (String.length niz -1) do
				print_char (String.get niz !c);
				c := !c +1;
				done;
				c := 0;
		done;
		
		print_string "\n"
	done;;

pod "banana";;


(*resitev*)
let pod niz =                                                                                                                                                                                                       
  for i=0 to (String.length niz -1) do
    for j=(String.length niz -1) downto i do
      for k=i to j do
        print_char (String.get niz k);
      done;
      print_string "\n"
    done;
  done;;

pod "banana";;

(*
banana , banan , bana , ban , ba , b , 
anana , anan , ana , an , a ,
nana , nan , na , n
ana , an , a , 
na , n , a
*)
