type contact = (string * string * int * string * string);;

let createContact f l a e p = (f, l, a, e, p);;
let formatContact (f, l, a, e, p) = (f, l, a, e, p);;
let getFirstName (f, _, _, _, _) = f;;
let getLastName (_, l, _, _, _) = l;;
let getAge (_, _, a, _, _) = a;;
let getEmail (_ , _, _, e, _) = e;;
let getPhone (_, _, _, _, p) = p;;
let sizelst l = List.fold_left (fun acc _ -> acc + 1) 0 l;;
let	strSub s x =
	if String.length s < x
		then s
	else
	let buff = Buffer.create 0
	in Buffer.add_string buff s ; Buffer.sub buff 0 x

let printID id = print_int id ; print_string " "
let printFirstName firstName = print_string (strSub firstName 16) ; print_string " "
let printLastName lastName = print_string (strSub lastName 16) ; print_string " "
let printAge age = print_string (strSub (string_of_int age) 4) ; print_string " "
let printEmail email = print_string (strSub email 32) ; print_string " "
let printPhone phone = print_string (strSub phone 14) ; print_endline ""

let printAll id contactT =
	printID id; printFirstName (getFirstName contactT) ; printLastName (getLastName contactT); printAge (getAge contactT) ; printEmail (getEmail contactT) ; printPhone (getPhone contactT)

let strCmpUnsensi refStr toCheck =
	let lowRef = String.lowercase_ascii refStr
	and lowToCheck = String.lowercase_ascii toCheck
	in String.compare lowRef lowToCheck

let cmpAllUnsensi refStr id tupleToCheck =
	if strCmpUnsensi refStr (getFirstName tupleToCheck) = 0 ||
		strCmpUnsensi refStr (getLastName tupleToCheck) = 0 ||
		strCmpUnsensi refStr (string_of_int id) = 0 ||
		strCmpUnsensi refStr (string_of_int (getAge tupleToCheck)) = 0 ||
		strCmpUnsensi refStr (getEmail tupleToCheck) = 0 ||
		strCmpUnsensi refStr (getPhone tupleToCheck) = 0
	then 0
	else -1
