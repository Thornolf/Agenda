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
	let buff = Buffer.create 0
	in Buffer.add_string buff s ; Buffer.sub buff 0 x
