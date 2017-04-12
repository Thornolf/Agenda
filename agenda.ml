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

    let printContacts lst whichfield str =
      match lst with
        | [] -> Printf.printf("AH");
        | x::xs -> match whichfield with
                    | All -> Printf.printf("Je print : All\n");
                    | Id -> Printf.printf("ID");
                    | FirstName -> Printf.printf("FirstName");
                    | LastName -> Printf.printf("LastName");
                    | Age -> Printf.printf("Age");
                    | Email -> Printf.printf("Email");
                    | Phone -> Printf.printf("Phone");
  end

  (* let drop list n =
      let rec aux i = function
        | [] -> []
        | h :: t -> if i = n then aux 1 t else h :: aux (i+1) t  in
      aux 1 list;; *)
