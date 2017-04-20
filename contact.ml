module type CONTACT =
	sig
		type contact = (string * string * int * string * string);;
		val createContact : string -> string -> int -> string -> string -> (string * string * int * string * string)
		val formatContact : string * string * int * string * string -> contact
		val getFirstName : contact -> string
		val getLastName : contact -> string
		val getAge : contact -> int
		val getEmail : contact -> string
		val getPhone : contact -> string
		val sizelst : 'a list -> int
		val strSub : string -> int -> string
		val addString : string -> int -> string
		val printElem : string -> int -> string
		val myStrStr : string -> String.t -> int
		val strCmpUnsensi : string -> string -> int
		val cmpAllUnsensi : string -> int -> contact -> int
		val allNumber : string -> bool
		val verifPhone : string -> bool

		val verifAge : int -> bool

		val printID : int -> unit
		val printFirstName : string -> unit
		val printLastName : string -> unit
		val printAge : int -> unit
		val printEmail : string -> unit
		val printPhone : string -> unit
		val printAll : int -> contact -> unit
	end

module Contact : CONTACT =
	struct
		type contact = (string * string * int * string * string);;
		let createContact f l a e p = (String.capitalize_ascii (String.lowercase_ascii f), String.uppercase_ascii l, a, e, p);;
		let formatContact (f, l, a, e, p) = (String.capitalize_ascii (String.lowercase_ascii f), String.uppercase_ascii l, a, e, p);;

		let	verifAge age =
			if age <= 0 || age >= 120 then false
			else true

		let getFirstName (f, _, _, _, _) = f;;
		let getLastName (_, l, _, _, _) = l;;
		let getAge (_, _, a, _, _) = a;;
		let getEmail (_ , _, _, e, _) = e;;
		let getPhone (_, _, _, _, p) = p;;
		let sizelst l = List.fold_left (fun acc _ -> acc + 1) 0 l;;
		let	strSub s x =
			if String.length s < x
				then s
			else
		let buff = Buffer.create 0
		in Buffer.add_string buff s ; Buffer.sub buff 0 x

		let addString str x =
			let buff = Buffer.create 0
			in Buffer.add_string buff str ;
			Buffer.add_string buff "                                    " ; Buffer.sub buff 0 x

		let printElem str x =
			if x < String.length str
				then strSub str x
			else
				addString str x

		let printID id = print_string (printElem (string_of_int id) 4)
		let printFirstName firstName = print_string (printElem firstName 16)
		let printLastName lastName = print_string (printElem lastName 16)
		let printAge age = print_string (printElem (string_of_int age) 4)
		let printEmail email = print_string (printElem email 32)
		let printPhone phone = print_string (printElem phone 14) ; print_endline ""

		let printAll id contactT =
			printID id; printFirstName (getFirstName contactT) ; printLastName (getLastName contactT); printAge (getAge contactT) ; printEmail (getEmail contactT) ; printPhone (getPhone contactT)

		let myStrStr str toFind =
			let rec loop index = match index with
				| index when String.length str < String.length toFind -> -1
				| index when String.length str - index < String.length toFind -> -1
				| _ -> if String.equal toFind (String.sub str index (String.length toFind)) = true then 0 else loop (index + 1)
			in loop 0

		let strCmpUnsensi refStr toCheck =
			let lowRef = String.lowercase_ascii refStr
			and lowToCheck = String.lowercase_ascii toCheck
			in myStrStr lowToCheck lowRef

		let cmpAllUnsensi refStr id tupleToCheck =
			if strCmpUnsensi refStr (getFirstName tupleToCheck) = 0 ||
				strCmpUnsensi refStr (getLastName tupleToCheck) = 0 ||
				strCmpUnsensi refStr (string_of_int id) = 0 ||
				strCmpUnsensi refStr (string_of_int (getAge tupleToCheck)) = 0 ||
				strCmpUnsensi refStr (getEmail tupleToCheck) = 0 ||
				strCmpUnsensi refStr (getPhone tupleToCheck) = 0
			then 0
			else -1

		let allNumber = function
			| str when str = "0" -> false
			| _ -> true

		let verifPhone  = function
			| str when String.get str 0 <> '0' -> false
			| str when String.length str <> 14 -> false
			| str when String.get str 2 <> ' ' && String.get str 5 <> ' ' && String.get str 8 <> ' ' && String.get str 11 <> ' ' -> false
			| str when allNumber str = false -> false
			| _ -> true

	end
