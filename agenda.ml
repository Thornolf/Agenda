
type field = All | Id | FirstName | LastName | Age | Email | Phone;;

module type AGENDA =
  sig
    val addContact : Contact.contact list -> string * string * int * string * string -> Contact.contact list
    (* val getContactId   : Contact.contact list -> field -> string -> int
    val removeContact  : Contact.contact list -> int -> Contact.contact list
    val replaceContact : Contact.contact list -> int -> string * string * int * string * string -> Contact.contact list *)
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
  end

  (* let drop list n =
      let rec aux i = function
        | [] -> []
        | h :: t -> if i = n then aux 1 t else h :: aux (i+1) t  in
      aux 1 list;; *)
