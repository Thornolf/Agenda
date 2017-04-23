module type EVENT =
	sig
		type event = (string * string * string * int * Contact.contact list)

	(* Fonction de création d'un event *)
		val createEvent : string -> string -> string -> int -> Contact.contact list -> event

		val formatEvent : string * string * string * int * Contact.contact list -> event

	(* Fonction de récupération des élément d'un event *)
		val getTitle : event -> string
		val getDate : event -> string
		val getTime : event -> string
		val getDur : event -> int
		val getContacts : event -> Contact.contact list

	(* Fonctions d'affichage des éléments d'un event *)
		val printTitle : string -> unit
		val printDate : string -> unit
		val printTime : string -> unit
		val printDuration : int -> unit
		val printInvited : Contact.contact list -> unit
		val printAllEvent : int -> event -> unit

		val cmpAll : string -> int -> event -> int

	end

module Event : EVENT =
	struct
		type event = (string * string * string * int * Contact.contact list)


		let	createEvent title date time dur cntList =
			(title, date, time, dur, cntList)

		let formatEvent (title, date, time, dur, cntList) =
			(String.capitalize_ascii (String.lowercase_ascii (String.trim title)), date, time, dur, cntList)

		let getTitle (title, _, _, _, _) = title
		let getDate (_, date, _, _, _) = date
		let getTime (_, _, time, _, _) = time
		let getDur (_, _, _, dur, _) = dur
		let getContacts (title, _, _, _, cntList) = cntList

		let printIDEvent id = print_string (string_of_int id) ; print_string " : "
		let printTitle title = print_string "\"" ; print_string title ; print_string "\" on "
		let printDate date = print_string date ; print_string " at "
		let printTime time = print_string time ; print_string " while "
		let printDuration dur = print_string (string_of_int dur) ; print_endline " min.\n Guests :"
		let rec printInvited  = function
			| [] -> print_string ""
			| x::xs -> Contact.printFirstName (Contact.getFirstName x) ; print_string " " ; Contact.printLastName (Contact.getLastName x) ; print_endline "" ; printInvited xs
		let printAllEvent id eventT =
			printIDEvent id ; printTitle (getTitle eventT) ; printDate (getDate eventT) ;
			printTime (getTime eventT) ; printDuration (getDur eventT) ; printInvited (getContacts eventT)

		let cmpAll refStr id tupleToCheck =
			if Contact.strCmpUnsensi refStr (string_of_int id) = 0 ||
			Contact.strCmpUnsensi refStr (getTitle tupleToCheck) = 0 ||
			Contact.strCmpUnsensi refStr (getDate tupleToCheck) = 0 ||
			Contact.strCmpUnsensi refStr (getTime tupleToCheck) = 0 ||
			Contact.strCmpUnsensi refStr (string_of_int (getDur tupleToCheck)) = 0
				then 0
			else -1
	end
