type contact

	val createContact : string -> string -> int -> string -> string -> (string * string * int * string * string)
	val formatContact : string * string * int * string * string -> contact
	val getFirstName : contact -> string
	val getLastName : contact -> string
	val getAge : contact -> int
	val getEmail : contact -> string
	val getPhone : contact -> string
	val sizelst : 'a list -> int
	val strSub : string -> int -> int -> string
	val myStrStr : string -> String.t -> int
	val strCmpUnsensi : string -> string -> int
	val cmpAllUnsensi : string -> int -> contact -> int
	val verifPhone : string -> bool
	val verifNumber : string -> int -> bool
	val verifMail : string -> bool
	val verifAge : int -> bool
	val makeHyphen : string -> char -> string
	val makeFirstName : string -> string
	val printID : int -> unit
	val printFirstName : string -> unit
	val printLastName : string -> unit
	val printAge : int -> unit
	val printEmail : string -> unit
	val printPhone : string -> unit
	val printAll : int -> contact -> unit
