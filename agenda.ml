type field = All | Id | FirstName | LastName | Age | Email | Phone;;

exception 	Remove_Impossible_On_An_Empty_List
exception	Remove_Using_An_Invalid_Id
exception 	Add_Contact_With_Invalid_Data

module type AGENDA =
  sig

	(* Permet d'ajouter un contact dans une liste de contacts *)
    val addContact : Contact.contact list -> string * string * int * string * string -> Contact.contact list

	(* Retourne l'ID d'un contact correspondant à certains critères *)
	val getContactId   : Contact.contact list -> field -> string -> int

	(* Permet de supprimer un contact d'une liste de contacts *)
	val removeContact  : Contact.contact list -> int -> Contact.contact list

	(* Permet remplacer un contact par un autre grâce à l'ID de ce dernier*)
    val replaceContact : Contact.contact list -> int -> string * string * int * string * string -> Contact.contact list

	(* Permet d'afficher un contact suivant certains critères *)
    val printContacts  : Contact.contact list -> field -> string -> unit
  end

module Agenda : AGENDA =
  struct

    let addContact lst newTuple =
		if Contact.getLastName newTuple = "" || Contact.getFirstName newTuple = ""
		|| Contact.verifAge (Contact.getAge newTuple) = false || Contact.verifPhone (Contact.getPhone newTuple) = false
		|| Contact.verifMail (Contact.getEmail newTuple) = false
			then raise (Add_Contact_With_Invalid_Data)
		else
      		let newco = [Contact.formatContact newTuple]
	  		in List.sort (fun first sec -> if first > sec then 1 else 0) (List.append lst newco)

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
		let rec loop acc = function
			| [] -> -1;
			| x::xs -> match fld with
				| All -> if Contact.cmpAllUnsensi str acc x = 0 then acc else loop (acc + 1) xs
				| Id -> if Contact.strCmpUnsensi str (string_of_int acc) = 0 then acc else loop (acc + 1) xs
				| FirstName -> if Contact.strCmpUnsensi str (Contact.getFirstName x) = 0 then acc else loop (acc + 1) xs
				| LastName -> if Contact.strCmpUnsensi str (Contact.getLastName x) = 0 then acc else loop (acc + 1) xs
				| Age -> if Contact.strCmpUnsensi str (string_of_int (Contact.getAge x)) = 0 then acc else loop (acc + 1) xs
				| Email -> if Contact.strCmpUnsensi str (Contact.getEmail x) = 0 then acc else loop (acc + 1) xs
				| Phone -> if Contact.strCmpUnsensi str (Contact.getPhone x) = 0 then acc else loop (acc + 1) xs
		in loop 0 lst

	let rec removeContact lst wch =
		if wch < 0 || wch > Contact.sizelst lst
			then raise (Remove_Using_An_Invalid_Id)
		else
			match lst with
				| [] -> raise (Remove_Impossible_On_An_Empty_List)
				| x::xs -> if wch = 0 then xs else x :: removeContact xs (wch - 1)

	let replaceContact lst id newCont =
		let newlist = removeContact lst id
		in addContact newlist newCont

    end
