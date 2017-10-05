not (1 = 2) && 1.0 < 2.0;; (* je true*)

let a = 1 and b =1.0;;
a+int_of_float(b);;

let sez1 = [1;2] and sez2= [4;5];;
let sez3 = sez1 @ [3] @ sez2;;
sez3;;

let zmnozektreh a b c = 
	let d = a  and e = b and f = c
	in d*e*f;;
zmnozektreh 1 2 3;;

let ime = "kek";;
let telefon = 041232333;;

let sez = [ime;string_of_int(telefon)];;

let floaty = 1.0 
and booly = true 
and stringy = "ads" 
and bolly = (true,false)
and floatintchar = (1.0,1,'1')
and charlistt = ['1']
and tupl = ((1,1),(1,1))
and tupple = (3,(true,('1',()),1.0),2,"kek")
and tut = (true,(1,(1.0,()),'1'))
and tututut = ("kek",("kekke",2),[1;3;3;4],true);;

let vecje a b =
	if a < b then b
	else a;;
vecje 1 2;;
vecje 2 1;;

let mala_crka = 's';;
let velika_crka = char_of_int(int_of_char(mala_crka)-32);;









