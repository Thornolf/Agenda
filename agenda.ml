
type field = All | Id | FirstName | LastName | Age | Email | Phone;;

module type AGENDA =
  sig
    val addContact : Contact.contact list -> string * string * int * string * string -> Contact.contact list
	val getContactId   : Contact.contact list -> field -> string -> int
	val removeContact  : Contact.contact list -> int -> Contact.contact list
    (*
    val replaceContact : Contact.contact list -> int -> string * string * int * string * string -> Contact.contact list
    *)
    val printContacts  : Contact.contact list -> field -> string -> unit
  end

module Agenda : AGENDA =
  struct
    let addContact list1 newTuple =
      let newco = [Contact.formatContact newTuple] in List.append list1 newco

    let rec printContacts lst whichfield str =
      match lst with
        | [] -> ();
        | x::xs -> match whichfield with
        	| All -> Printf.printf "%d %s %s %d %s %s" 0 (Contact.getFirstName x) (Contact.getLastName x) (Contact.getAge x) (Contact.getEmail x) (Contact.getPhone x) ; printContacts xs whichfield str
            | Id -> Printf.printf "ID"
            | FirstName -> Printf.printf "%s\n" (Contact.getFirstName x) ; printContacts xs whichfield str
            | LastName -> Printf.printf "%s\n" (Contact.getLastName x) ; printContacts xs whichfield str
            | Age -> Printf.printf "%d\n" (Contact.getAge x) ; printContacts xs whichfield str
            | Email -> Printf.printf "%s\n" (Contact.getEmail x) ; printContacts xs whichfield str
            | Phone -> Printf.printf "%s\n" (Contact.getPhone x) ; printContacts xs whichfield str

(*ex: String = "Robert" && field = "FirstName" // On va donc rechercher dans tous les field FirstName le string Robert*)

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

	
    end
