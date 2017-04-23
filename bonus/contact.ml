type contact = (string * string * int * string * string);;

		let	strSub str limit beg =
			if String.length str < limit
				then str
			else if String.length str < beg
				then str
			else
				let buff = Buffer.create 0
				in Buffer.add_string buff str ; Buffer.sub buff beg limit

		let makeHyphen str chr =
			if String.contains str chr = false
				then str
			else if String.length str < String.index str chr
				then str
			else
				let partA = strSub str ((String.index str chr) + 1) 0
				and partB = strSub str (((String.length str) - String.index str chr) - 1) ((String.index str chr) + 1)
				in partA ^ (String.capitalize_ascii partB)

		let makeFirstName = function
			| hyphenName when String.contains hyphenName '-' = true -> makeHyphen hyphenName '-'
			| spaceName when String.contains spaceName ' ' = true -> makeHyphen spaceName ' '
			| aposName when String.contains aposName '\'' = true -> makeHyphen aposName '\''
			| str -> makeHyphen str ' '

		let createContact f l a e p =
			(f, l, a, e, p)

		let formatContact (f, l, a, e, p) =
			(String.capitalize_ascii (makeFirstName (String.lowercase_ascii (String.trim f))), String.uppercase_ascii (String.trim l), a, e, p)

		let	verifAge age =
			if age <= 0 || age >= 120
				then false
			else true

		let getFirstName (f, _, _, _, _) = f;;

		let getLastName (_, l, _, _, _) = l;;

		let getAge (_, _, a, _, _) = a;;

		let getEmail (_ , _, _, e, _) = e;;

		let getPhone (_, _, _, _, p) = p;;

		let sizelst l = List.fold_left (fun acc _ -> acc + 1) 0 l;;

		let addString str x =
			let buff = Buffer.create 0
			in Buffer.add_string buff str ;
			Buffer.add_string buff "                                    " ; Buffer.sub buff 0 x

		let formatElem str x =
			if x < String.length str
				then strSub str x 0
			else
				addString str x

		let printID id = print_string (formatElem (string_of_int id) 4)

		let printFirstName firstName = print_string (formatElem firstName 16)

		let printLastName lastName = print_string (formatElem lastName 16)

		let printAge age = print_string (formatElem (string_of_int age) 4)

		let printEmail email = print_string (formatElem email 32)

		let printPhone phone = print_string (formatElem phone 14) ; print_endline ""

		let printAll id contactT =
			printID id; printFirstName (getFirstName contactT) ; printLastName (getLastName contactT) ; printAge (getAge contactT) ;
			printEmail (getEmail contactT) ; printPhone (getPhone contactT)

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

		let rec verifNumber str idx =
			match String.get str idx with
				| '0' .. '9' when idx = ((String.length str) - 1) -> true
				| '0' .. '9' -> true && verifNumber str (idx + 1)
				| ' ' when idx = 2 || idx = 5 || idx = 8 || idx = 11 -> true && verifNumber str (idx + 1)
				| _ -> false

		let verifPhone  = function
			| str when String.get str 0 <> '0' -> false
			| str when String.length str <> 14 -> false
			| str when String.get str 2 <> ' ' && String.get str 5 <> ' ' && String.get str 8 <> ' ' && String.get str 11 <> ' ' -> false
			| str when verifNumber str 0 = false -> false
			| _ -> true

		let verifMail = function
			| str when String.length str < 5 -> false
			| str when (String.index str '@') <= 0 -> false
			| str when (String.index str '@') <> (String.rindex str '@') -> false
			| str when (String.rindex str '.' ) = ((String.length str) - 1) -> false
			| str when (String.rindex str '.') < (String.rindex str '@') -> false
			| str when (String.rindex str '.') = ((String.rindex str '@') + 1) -> false
			| _ -> true
