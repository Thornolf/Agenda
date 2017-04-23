type event

val createEvent : string -> string -> string -> int -> Contact.contact list -> event
val formatEvent : string * string * string * int * Contact.contact list -> event
val getTitle : event -> string
val getDate : event -> string
val getTime : event -> string
val getDur : event -> int
val getContacts : event -> Contact.contact list
val printTitle : string -> unit
val printDate : string -> unit
val printTime : string -> unit
val printDuration : int -> unit
val printInvited : Contact.contact list -> unit
val printAllEvent : int -> event -> unit
val cmpAll : string -> int -> event -> int
