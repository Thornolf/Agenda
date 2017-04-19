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

    let printContacts lst whichfield str =
    	let rec loop acc = function
    		| [] -> print_string ""
    		| x::xs when str = "" -> Contact.printAll acc x ; loop (acc + 1) xs
    		| x::xs -> match whichfield with
    			| All -> if Contact.cmpAllUnsensi str acc x = 0 then Contact.printAll acc x ; loop (acc + 1) xs
    			| Id -> if Contact.strCmpUnsensi str (string_of_int acc) = 0 then Contact.printAll acc x ; loop (acc + 1) xs
    			| FirstName -> if Contact.strCmpUnsensi str (Contact.getFirstName x) = 0 then Contact.printAll acc x ; loop (acc + 1) xs
    			| LastName -> if Contact.strCmpUnsensi str (Contact.getLastName x) = 0 then Contact.printAll acc x ; loop (acc + 1) xs
    			| Age -> if Contact.strCmpUnsensi str (string_of_int (Contact.getAge x)) = 0 then Contact.printAll acc x; loop (acc + 1) xs
    			| Email -> if Contact.strCmpUnsensi str (Contact.getEmail x) = 0 then Contact.printAll acc x ; loop (acc + 1) xs
    			| Phone -> if Contact.strCmpUnsensi str (Contact.getPhone x) = 0 then Contact.printAll acc x ; loop (acc + 1) xs
    	in loop 0 lst

    let getContactId lst fld str =
		let rec loop acc =
			function
				| [] -> -1;
				| x::xs -> match fld with
					| All -> if Contact.cmpAllUnsensi str acc x == 0 then acc else loop (acc + 1) xs
					| Id -> acc
					| FirstName -> if Contact.strCmpUnsensi str (Contact.getFirstName x) == 0 then acc else loop (acc + 1) xs
					| LastName -> if Contact.strCmpUnsensi str (Contact.getLastName x) == 0 then acc else loop (acc + 1) xs
					| Age -> if Contact.strCmpUnsensi str (string_of_int (Contact.getAge x)) == 0 then acc else loop (acc + 1) xs
					| Email -> if Contact.strCmpUnsensi str (Contact.getEmail x) == 0 then acc else loop (acc + 1) xs
					| Phone -> if Contact.strCmpUnsensi str (Contact.getPhone x) == 0 then acc else loop (acc + 1) xs
		in loop 0 lst

	let rec removeContact lst wch = match lst with
		| [] -> []
		| x::xs -> if wch = 0 then xs else x :: removeContact xs (wch - 1)


	let replaceContact lst id newCont =
		let newlist = removeContact lst id
		in addContact newlist newCont
    end
