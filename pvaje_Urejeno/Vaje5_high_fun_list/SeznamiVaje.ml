let sez = [454;5656];;
let rec nasMap f sez=
	if (sez =[]) then []
	else (f (List.hd sez)) :: nasMap f (List.tl sez);;    

nasMap (fun x-> x+1) sez;;
 (*vse elemente ki dobimo vstavlamo rekurzivno v nasmap in spet f sibamo cez in rep od seznama*)

let rec kvadriraj sez =
if (sez = []) then []  (*//zaustaviten pogoj, ko je seznam prazen,generiraj prazn seznam*)
else [List.hd sez * List.hd sez] @ kvadriraj (List.tl sez);;   (*//glavo iz seznama in pomnozite z samo sabo*)

let rec mnozi sez x =
	if (sez = []) then [] (*#//zgeneriramo strukturo*)
	else [(List.hd sez) *x] @ mnozi (List.tl sez)  x;;(*#//vzamemo prvi element pomnozimo z x in vstavimo rekurzivno v pomnozen  rep z ostalim seznamom*)
	

