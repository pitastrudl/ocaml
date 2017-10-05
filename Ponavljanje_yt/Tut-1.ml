let i = 9 and j = 10 in
  if i < j then
  		true
  	else
  		false;;   (*returns true, if i is larger than j*)


let i = 9 and j = 10;;
i < j;;


(*-------oklepaji--------------*)
(*tukaj lahko delamo z oklepaji ali pa being/end*)
if i < j then
	 (
		let x = 10 in
			x + i 
		)
		else
			begin
				let f = 3 in
					f+4
			end;;
(*-------------------krneki--*)
let i = 9 and j = 1;;
if i < j || false then
	 (
		let x = 10 in
			x + 2 
		)
		else 
			begin
				let f = 3 in
					f+4
			end;;
(*---------------------*)
(*---------------------*)

let x = 4 and y = 4;;
x = y;;  (*deep equal,true, same value*)
x == y;; (*shallow equal, same value*)
x <> y;; (*deep not equal, same value*)
let s = "string" and u = "string";;
s = u;; (*deep equal, same value*)
s != u;;  (*shallo6w not equal,true,becuase, *)
s <> u;; (*deep not equal, false*)

(*for, while loop----------------------------------------------------*)
for i = 0 to 10 do
	print_string "value of i is: ";
	print_int i;
	print_string "\n";
	done;; 



for i = 10 downto 0 do
	print_string "value of i is: ";
	print_int i;
	print_string "\n";
	done;; 


let s = print_string "test";;  (*ne naredi nic*)
s;; 


(*mutable var*)
let x = 5;;
let t = ();;
let x = ref 5;;  (*referenca zato da lahko spreminjamo*)
(*ko postavimo referenco na variable, se tip variable spremeni in jo morammo tretirati drugace*)
x;;
!x;;  (*actual value*)

let s = ref "str";;  
!s;;

x := 3;; (*nacin priredbe*)
!x;; 

s := !s ^ "test";; (*priredili vrednost*)
!s;;
(*ce ne damo klicaja pokaze referenco ki je ta variable, zato das klicaj za dejansko vrednost*)

while !x < 10 do
	print_string "Hello world\n";
	x := !x +1;  (*tako prirejas vrednost x-a*)
	done;;
!x;;



let f1' = ref 3.4 and f2' = ref 3.4;;  (*dve float referece variabli*)


f1' = f2';; (*true becuase content vise are the same, comparing phisically*)
f1' == f2';;(*false, becuase they are not exactly the same.two different variables*)

f1' <> f2';; 
f1' != f2';; 


(*funkcijeeee--------------------*)

let max x y = if x > y then x else y;;  (*apostrof pomeni polimorfizem*)
max 5 6;;
max 6.7 8.9;;
max true false;;
max 'a' 'f';;
max "String1" "Hello";;
max 3.4 5;; (*ne dela!*)


(*anonymous functions/lambda functions*)
(*lahko parsamo funckije kot argumente, lahko tudi returnamo funkcije, lahko jih damo tudi v data strukture*)
(* currying*)

fun x y -> if x > y then x else y;;  (*funkcija*)  

(fun x y -> if x > y then x else y) 4 5;;   (*damo v oklepaj in se v bgju izvede nova funkcija ki vzame potem parametre*)

let max = fun x y -> if x > y then x else y;; (*definiramo funkcijo kot expression*)

max 4 5;;

let min x y = if x < y then x else y;;

min 5 6;;

(*visje funkcije in currying, parsing functions into functions*) 

let my_function f x y = f x y;; (*f je funkcija, ki vzame argumente x in y*)
my_function  (fun x y -> if x > y then x else y) 5 6;;
my_function max 8 9;;  (*lahko za f vzames katerikoli funkicjo*)


(*curryng, vzames funkcijo z vec argumenti in potem vsak argument postane svoja funkcija*)
let multiply x y = x * y;;  
multiply 2.5 2.5;;(* ne gre,rabis dolocit v operaciji *.  da bo sprejemal floaterabi int*)
multiply 2 2 ;;

(multiply 5) 6;; (*currying, se naredi nova funkcija v ozadju in vzame naslednji argument ce je tam*)

let multiply5 = multiply 5;;
multiply5 6;;

(*n-terice, tuples,prazna nterica je unit----------------------------------------------------------------*)
(*vrstni red je pomemben, prvi je int, drugi str in tretji bool, ce so drugacni*)

let max a b = if a > b then a else b;;   (*za argumente tudi lahko vzamemo nterke*)
let max (a,b) = if a > b then a else b;;

max (4,5);;

let my_fun(i,s,b) = if i > 10 then print_int i else 
	if s = "test" then print_string s else
		if b then print_string "true";;

my_fun(44,"hi",true);;

let f = my_fun (44,"hi",true) in ();; 

let _ = my_fun (44,"hi",true) in ();; (*da zgoraj izenacmo errror smo dal karkoli*)


let fst_of_3tuple (a,b,c) = a;;
let thd_of_3tuple (a,b,c) = c;;

fst_of_3tuple (44,"hi",true);;
thd_of_3tuple (44,"hi",true);;

let my_tuple = (44,"hi",true);;

my_fun my_tuple;;


(*match expression, pattern matching-------------------------*)
(*ce je v pattern matchingu *)
let int_to_month (i : int) : string = match i with  (*i : int, pomeni da mora bit int*)
| 1 -> "Jan"
| 2 -> "Feb"
| 3 -> "Mar"
| 4 -> "Apr"
| 5 -> "Maj"
| 6 -> "Jun"
| 7 -> "Jul"
| 8 -> "Aug"
| 9 -> "Sept"
| 10 -> "Oct"
| 11 -> "Nov"
| 12 -> "Dec"
| _ -> "Wrong number";;
int_to_month 5;;
int_to_month 13;;
int_to_month "s";;

(*  *)
(* *)
(* *)
(* *)

(*recursiveeee-----------------------------*)
(*rekurzija vedno ZAUSSTAVITVENI POGOJ *)

let rec fact n =  (*fakulteta*)
	if n == 1 then
		1
	else n * fact(n-1);;

fact 5;;

(*fibonacci z normalno funkcijo*)
let rec fibonacci n = 
	if n < 2 then
		1
	else
		fibonacci(n-1) + fibonacci(n-2);;

fibonacci 10;;

(*pattern matching fibonacci*)
let rec fib x =
	match x with 
	| 0 -> 1
	| 1 -> 1
	| _ -> fib(x-1) + fib(x-2);;
fib 10;;

(*funkcija v funkciji, gre v inf loop, rip*)
let rec fun1 x = 
	match x with
	| 0 -> "Zero"
	| _ -> fun2 x
and
	fun2 y =  (*je tudi rekurzivna*)
		match y with
		| 1 -> "One"
		| _ -> fun1 (y-1);;

fun1 0;; 
fun1 1;; 
fun1 5;;


(**)
(* *)
(* *)
(* tracing function--------------------------*)
(*sledi funkciji, za debug pomoje *)
(* *)
#trace fun1;;

fun1 10;;

#untrace fun1;;

(*ocaml moduli*)

List.hd [1;2;3;4];;
List.tl [1;2;3;4];;

(*string module-------------------------*)

String.get "Test" 3;;
String.length "String";;

let s = "we are checking the string module";;
String.length s;;

String.sub s 3 3;; (*vrne string od lokacije 3 in dolzino tega*)
String.sub s 3 12;;

let l = ["bljad";"suka";"hhueuhehueuh"];;
String.concat " " l;;  (*z seperatorjem kontaktiniras stringe*)

String.iter (fun x -> print_char x) s;;  (*applies an anonymous function to all characters,doesnt return anything*)

String.map (fun x -> Char.uppercase x) s;; (*returns a new fresh string wooooo*)

(*odpreti module za uporabo funkcij brez klicanja modula.*)
open Char;;
open String;;
uppercase 'c';;

let s2upper = String.uppercase;;
s2upper "s";; 

(*vec o spremenljivkah n funkcijah*)

let a = 4;;
let a_ = 4;;
let _a = 5;;

let () = print_int 5;;

(*gnezdene funkcije*)
(* *)
(* ne bomo vidl my_fun, je lokalna*)
let multipl5 x =  (*25*)
	let v = x * 5 
			in  (*v=25*)
		let my_fun y = y * y  (*parsamo 25*) 
				in
		let f = my_fun 4 (*bo vrnilo 16*)
					in
				f * v;;  (*16*25=400*)

multipl5 5;;

(*scoping*)

let j = 4;;
let mulj t =
	j * t ;;

let j=5;;

mulj 4;;
(*scoping je da ce recimo buildas variable in potem ta variable uporabs v funkciji, se bo ta variable uporabljal v funkciji*)
(* ce ga bos kasneje spreminjal ta isti variable, moras potem tudi funkcijo se enkrat zgraditi*)


(*labels and optional parameters in ocaml functions*)

let my_funs ~x:i ~y:j =  (*x in y sta labeli, i pa jv kodi*)
	i * j;;
	
my_funs ~x:4 ~y:5;;  (*vrstni red ni pomemben*)

let my_funs ~x ~y =
	x - y;;

let x = 7 and y = 8 in 
	my_funs ~x ~y;; 

let x = 7 and y = 8 in my_funs ~y ~x;;

(*optional paramteters*)

let my_funs ?(x=3) y =  (*ce ni x-a podanga bo x 3*)
	y *x;;
my_funs 5;;

my_funs ~x:6 5;;

my_funs 5 ~x:6;;


(*ocam*)



















	
	
	
	
	
