(*Funkcije v OCAml*)

function x -> x * x;;
function x -> x -1;;
function (a,b) -> a*b;;
(function x -> x * x) 9;;
(function x -> x -1) 6;;
(function (a,b) -> a*b) (1,3);;
(fun x -> x + 1) 9;;
let naslednik = fun x -> x+1;;
let predhodnik = fun x -> x-1;;
naslednik 10;;
predhodnik 10;;
let kvadrat = fun x -> x * x in kvadrat 9;;
kvadrat 9;;
let naslednik a = a +1;;
let predhodnik b = b -1;;
let nekaj (a,b) = a * b;;
nekaj (8,3);;
let nekajD a b = a * b;;
nekajD 8 3;;
let nekajD = fun a -> fun b -> a* b;;
let naredi1 x = (x-1,x+1);;
let naredi2 x = (predhodnik x, naslednik x);;
naredi1 9;;
naredi2 9;;
let rec sestejDo n = 
	if(n = 0) then 0
	else n + sestejDo (n-1);;
sestejDo 9;;
sestejDo 3;;
let rec fak n =
	if(1 < n) then failwith "bla"
	else if ( n = 1) then 1
	else n * fak (n-1);;
fak 0;;
let rec fib n =
	if (n <=2) then 1
	else fib (n-1) + fib (n-2);;
fib 9;;
fib 20;;
fib 40;;
(*naloge iz pdf-spletna ucilnica*)
let rec vSez n =
	if (n = 0) then []
	else vSez (n-1) @ [n];;
vSez 7;;
let rec vSezF n =
	if (n = 0) then []
	else vSezF (n-1) @ [fib n];;
vSezF 7;;
List.tl[8];;
let rec sestej sez =
	if (sez = []) then 0
	List.hd sez + sestej (List.tl sez);;
sestej [7;4;2;2;5;6];;
sestej (vSezF 9);;
(*Funkcija ki obrne seznam*)
let rec obrni sez =
	if (sez = []) then []
	else obrni (List.tl sez) @ [List.hd sez];;
obrni [7;4;2;5;6];;
obrni (vSezF 20);;