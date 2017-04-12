open Contact;;
open Agenda;;

let tupl1 = ("Jack", "Bird", 30, "Jack@Bird.eu", "01")
let tupl2 = ("Jason", "Bourne", 24, "Jason@Bourne.eu", "02")

let a = []
let b = []

(* Agenda.addContact l ex;; *)
  let ex = Contact.createContact "AAA" "AAA" 43 "AAA" "AAA"

  let format = Contact.formatContact ex
  (* let l = [] in *)
  (* begin *)
    let add = Agenda.addContact a ex

  let a () = Agenda.printContacts add FirstName ""
  (* end *)

  (* Contact.createContact "Jack" "Ichan" 42 "l@gross.bit" "0123456789" *)

(* let a = createContact "Jack" "Bird" 30 "lol" "01" *)
