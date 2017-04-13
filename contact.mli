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
