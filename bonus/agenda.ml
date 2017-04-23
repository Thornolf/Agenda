type field = All | Id | FirstName | LastName | Age | Email | Phone;;
type fieldEvent = AllEvent | IdEvent | Title | Date | Time | Duration;;

exception 	Remove_Impossible_On_An_Empty_List
exception	Remove_Using_An_Invalid_Id
exception 	Add_Contact_With_Invalid_Data

module type AGENDA =
  sig

	(* Permet d'ajouter un contact dans une liste de contacts *)
    val addContact : Contact.contact list -> string * string * int * string * string -> Contact.contact list

	(* Permet d'ajouter un event dans une liste d'event *)
	val addEvent : Event.event list -> string * string * string * int * Contact.contact list -> Event.event list

	(* Retourne l'ID d'un contact correspondant à certains critères *)
	val getContactId   : Contact.contact list -> field -> string -> int

	(* Retourne l'ID d'un event selon des critère donnés *)
	val getEventId	: Event.event list -> fieldEvent -> string -> int

	(* Permet de supprimer un contact d'une liste de contacts *)
	val removeContact  : Contact.contact list -> int -> Contact.contact list

	(* Permet de supprimer un event d'une liste d'events *)
	val removeEvent  : Event.event list -> int -> Event.event list

	(* Permet remplacer un contact par un autre grâce à l'ID de ce dernier*)
    val replaceContact : Contact.contact list -> int -> string * string * int * string * string -> Contact.contact list

	(* Permet d'afficher un contact suivant certains critères *)
    val printContacts  : Contact.contact list -> field -> string -> unit

	(* Permet d'afficher un event suivant certaines conditions *)
	val printEvents : Event.event list -> fieldEvent -> string -> unit
  end

module Agenda : AGENDA =
  struct

    let addContact lst newTuple =
	let newco =
		if Contact.getLastName (Contact.formatContact newTuple) = "" || Contact.getFirstName (Contact.formatContact newTuple) = ""
		|| Contact.verifAge (Contact.getAge (Contact.formatContact newTuple)) = false || Contact.verifPhone (Contact.getPhone (Contact.formatContact newTuple)) = false
		|| Contact.verifMail (Contact.getEmail (Contact.formatContact newTuple)) = false
			then raise (Add_Contact_With_Invalid_Data)
		else
			[Contact.formatContact newTuple]
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

	let addEvent lst newTuple =
		let newEv = [Event.formatEvent newTuple]
		in List.sort (fun first sec -> if Event.getTitle first > Event.getTitle sec then 1 else 0) (List.append lst newEv)

	let printEvents lst eventField str =
		let rec loop acc = function
			| [] -> print_string ""
			| x::xs when str = "" -> Event.printAllEvent acc x ; loop (acc + 1) xs
			| x::xs -> match eventField with
				| AllEvent -> if Event.cmpAll str acc x = 0 then Event.printAllEvent acc x ; loop (acc + 1) xs
				| IdEvent -> if Contact.strCmpUnsensi str (string_of_int acc) = 0 then Event.printAllEvent acc x ; loop (acc + 1) xs
				| Title -> if Contact.strCmpUnsensi str (Event.getTitle x) = 0 then Event.printAllEvent acc x ; loop (acc + 1) xs
				| Date -> if Contact.strCmpUnsensi str (Event.getDate x) = 0 then Event.printAllEvent acc x ; loop (acc + 1) xs
				| Time -> if Contact.strCmpUnsensi str (Event.getTime x) = 0 then Event.printAllEvent acc x ; loop (acc + 1) xs
				| Duration -> if Contact.strCmpUnsensi str (string_of_int (Event.getDur x)) = 0 then Event.printAllEvent acc x ; loop (acc + 1) xs
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

	let getEventId lst eventField str =
		let rec loop acc = function
			| [] -> -1
			| x::xs -> match eventField with
				| AllEvent -> if Event.cmpAll str acc x = 0 then acc else loop (acc + 1) xs
				| IdEvent -> if Contact.strCmpUnsensi str (string_of_int acc) = 0 then acc else loop (acc + 1) xs
				| Title -> if Contact.strCmpUnsensi str (Event.getTitle x) = 0 then acc else loop (acc + 1) xs
				| Date -> if Contact.strCmpUnsensi str (Event.getDate x) = 0 then acc else loop (acc + 1) xs
				| Time -> if Contact.strCmpUnsensi str (Event.getTime x) = 0 then acc else loop (acc + 1) xs
				| Duration -> if Contact.strCmpUnsensi str (string_of_int (Event.getDur x)) = 0 then acc else loop (acc + 1) xs
		in loop 0 lst

	let rec removeContact lst wch =
		if wch < 0 || wch > Contact.sizelst lst
			then raise (Remove_Using_An_Invalid_Id)
		else
			match lst with
				| [] -> raise (Remove_Impossible_On_An_Empty_List)
				| x::xs -> if wch = 0 then xs else x :: removeContact xs (wch - 1)

	let rec removeEvent lst wch =
		if wch < 0 || wch > Contact.sizelst lst
			then raise (Remove_Using_An_Invalid_Id)
		else
			match lst with
				| [] -> raise (Remove_Impossible_On_An_Empty_List)
				| x::xs -> if wch = 0 then xs else x :: removeEvent xs (wch - 1)

	let replaceContact lst id newCont =
		let newlist = removeContact lst id
		in addContact newlist newCont

    end
