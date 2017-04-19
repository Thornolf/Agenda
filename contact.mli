type contact

val createContact : string -> string -> int -> string -> string -> (string * string * int * string * string)
val formatContact : string * string * int * string * string -> contact
val getFirstName : contact -> string
val getLastName : contact -> string
val getAge : contact -> int
val getEmail : contact -> string
val getPhone : contact -> string
val sizelst : 'a list -> int
val strSub : string -> int -> string
val strCmpUnsensi : string -> string -> int
val cmpAllUnsensi : string -> int -> contact -> int

val printID : int -> unit
val printFirstName : string -> unit
val printLastName : string -> unit
val printAge : int -> unit
val printEmail : string -> unit
val printPhone : string -> unit
val printAll : int -> contact -> unit
