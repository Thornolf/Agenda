type contact = (string * string * int * string * string);;

let createContact f l a e p = (f, l, a, e, p);;
let formatContact (f, l, a, e, p) = (f, l, a, e, p);;
let getFirstName (f, _, _, _, _) = f;;
let getLastName (_, l, _, _, _) = l;;
let getAge (_, _, a, _, _) = a;;
let getEmail (_ , _, _, e, _) = e;;
let getPhone (_, _, _, _, p) = p;;
