type field = All | Id | FirstName | LastName | Age | Email | Phone;;
type fieldEvent = AllEvent | IdEvent | Title | Date | Time | Duration;;

module type AGENDA =
  sig
    val addContact : Contact.contact list -> string * string * int * string * string -> Contact.contact list
    val getContactId   : Contact.contact list -> field -> string -> int
	val removeContact  : Contact.contact list -> int -> Contact.contact list
    val replaceContact : Contact.contact list -> int -> string * string * int * string * string -> Contact.contact list
    val printContacts  : Contact.contact list -> field -> string -> unit

	val addEvent : Event.event list -> string * string * string * int * Contact.contact list -> Event.event list
	val printEvents : Event.event list -> fieldEvent -> string -> unit
	val getEventId	: Event.event list -> fieldEvent -> string -> int
	val removeEvent  : Event.event list -> int -> Event.event list
  end

module Agenda : AGENDA
