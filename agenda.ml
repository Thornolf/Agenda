type field = All | Id | FirstName | LastName | Age | Email | Phone;;

module type AGENDA =
  sig
    val addContact : Contact.contact list -> string * string * int * string * string -> Contact.contact list
	val getContactId   : Contact.contact list -> field -> string -> int
	val removeContact  : Contact.contact list -> int -> Contact.contact list
    val replaceContact : Contact.contact list -> int -> string * string * int * string * string -> Contact.contact list
    val printContacts  : Contact.contact list -> field -> string -> unit
  end

module Agenda : AGENDA =
  struct
    let addContact list1 newTuple =
      let newco = [Contact.formatContact newTuple] in List.append list1 newco

    (* let printContacts lst whichfield str =
		let rec loop acc = function
        	| [] -> print_string ""
			| x::xs when str = "" -> Contact.printAll acc x ; loop (acc + 1) xs
        	| x::xs -> match whichfield with
				| All -> print_endline "all" ; loop (acc + 1) xs
				| Id -> if Contact.strCmpUnsensi str (string_of_int acc) = 0 then Contact.printAll acc x ; loop (acc + 1) xs
				| FirstName -> if Contatc ; loop (acc + 1) xs
				| LastName -> print_endline "last" ; loop (acc + 1) xs
				| Age -> print_endline "age" ; loop (acc + 1) xs
				| Email -> print_endline "mail" ; loop (acc + 1) xs
				| Phone -> print_endline "phone" ; loop (acc + 1) xs
		in loop 0 lst *)

    let printContacts lst whichfield str =
    let rec loop acc = function
    | [] -> print_string ""
    | x::xs when str = "" -> Contact.printAll acc x ; loop (acc + 1) xs
    | x::xs -> match whichfield with
    | All -> print_endline "all" ; loop (acc + 1) xs
    | Id -> if Contact.strCmpUnsensi str (string_of_int acc) = 0 then Contact.printAll acc x ; loop (acc + 1) xs
    | FirstName -> if Contatc.strCmpUnsensi str (Contact.getFirstName x) = 0 then Contact.printAll acc x ; loop (acc + 1) xs
    | LastName -> print_endline "last" ; loop (acc + 1) xs
    | Age -> print_endline "age" ; loop (acc + 1) xs
    | Email -> print_endline "mail" ; loop (acc + 1) xs
    | Phone -> print_endline "phone" ; loop (acc + 1) xs
    in loop 0 lst

    let getContactId lst fld str =
		let rec loop lst fld str acc =
			match lst with
				| [] -> -1;
				| x::xs -> match fld with
					| All -> if str = Contact.getFirstName x || str = Contact.getLastName x || (int_of_string str) = Contact.getAge x || str = Contact.getEmail x || str = Contact.getPhone x then acc else loop xs fld str acc + 1
					| Id -> acc
					| FirstName -> if str = Contact.getFirstName x then acc else loop xs fld str acc + 1
					| LastName -> if str = Contact.getLastName x then acc else loop xs fld str acc + 1
					| Age -> if (int_of_string str) = Contact.getAge x then acc else loop xs fld str acc + 1
					| Email -> if str = Contact.getEmail x then acc else loop xs fld str acc + 1
					| Phone -> if str = Contact.getPhone x then acc else loop xs fld str acc + 1
		in loop lst fld str 0

	let rec removeContact lst wch = match lst with
		| [] -> []
		| x::xs -> if wch = 0 then xs else x :: removeContact xs (wch - 1)


	let replaceContact lst id newCont =
		let newlist = removeContact lst id
		in addContact newlist newCont
    end
